#ifndef SOLVER_CACHE_H
#define SOLVER_CACHE_H

#include <memory>
#include <any>
#include <future>
#include <unordered_set>
#include "../internal/SolverCancelled.h"
#include "../internal/CandidatesId.h"
#include "../internal/DependenciesId.h"
#include "../internal/FrozenCopyMap.h"
#include "../internal/Arena.h"
#include "../internal/FrozenMap.h"
#include "../Pool.h"
#include "../Problem.h"
#include "../Common.h"
#include "Clause.h"
#include "WatchMap.h"

// Keeps a cache of previously computed and/or requested information about solvables and version
// sets.
template<typename VS, typename N, typename D>
class SolverCache {
public:
    D provider;

//    std::pair<VS, N> _data;

    // A mapping of `Requirement` to a sorted list of candidates that satisfy it.
    FrozenMap<Requirement, std::vector<SolvableId>> requirement_to_sorted_candidates;

    explicit SolverCache(D provider) : provider(provider) {}

    // Returns a reference to the pool used by the solver
    std::shared_ptr<Pool<VS, N>> get_pool() {
        return provider.pool;
    }

    // Returns the candidates for the package with the given name. This will either ask the
    // [`DependencyProvider`] for the entries or a cached value.
    // If the provider has requested the solving process to be cancelled, the cancellation value
    // will be returned as an `Err(...)`.
    const PackageCandidates& get_or_cache_candidates(NameId package_name) {
        // If we already have the candidates for this package cached we can simply return
        auto optional_candidates_id = package_name_to_candidates.get_copy(package_name);
        if (optional_candidates_id.has_value()) {
            return candidates[optional_candidates_id.value()];
        } else {
            // Since getting the candidates from the provider is a potentially blocking
            // operation, we want to check beforehand whether we should cancel the solving
            // process
            check_cancelled();
            auto package_candidates = provider.get_candidates(package_name).value_or(PackageCandidates());
            check_cancelled();
            for (const auto solvable : package_candidates.hint_dependencies_available) {
                const auto index = solvable.to_usize();
                if (hint_dependencies_available.size() <= index) {
                    hint_dependencies_available.resize(index + 1, false);
                }
                hint_dependencies_available[index] = true;
            }
            auto package_candidates_id = candidates.alloc(package_candidates);
            package_name_to_candidates.insert_copy(package_name, package_candidates_id);
            return candidates[package_candidates_id];

        }
    }

    // Returns the candidates of a package that match the specified version set.
    // If the provider has requested the solving process to be cancelled, the cancellation value
    // will be returned as an `Err(...)`.
    std::vector<SolvableId> get_or_cache_matching_candidates(VersionSetId version_set_id) {
        auto temp_candidates = version_set_candidates.get(version_set_id);
        if (temp_candidates.has_value()) {
            return temp_candidates.value();
        } else {
            auto package_name = get_pool()->resolve_version_set_package_name(version_set_id);
            auto version_set = get_pool()->resolve_version_set(version_set_id);
            auto package_candidates = get_or_cache_candidates(package_name);
            std::vector<SolvableId> matching_candidates;
            for (auto p : package_candidates.candidates) {
                auto version = get_pool()->resolve_internal_solvable(p).get_solvable_unchecked().get_inner();
                if (version_set.contains(version)) {
                    matching_candidates.push_back(p);
                }
            }
            version_set_candidates.insert(version_set_id, matching_candidates);
            return matching_candidates;
        }
    }

    // Returns the candidates that do *not* match the specified requirement.
    // If the provider has requested the solving process to be cancelled, the cancellation value
    // will be returned as an `Err(...)`.
    std::vector<SolvableId> get_or_cache_non_matching_candidates(VersionSetId version_set_id) {
        auto optional_candidates = version_set_inverse_candidates.get(version_set_id);
        if (optional_candidates.has_value()) {
            return optional_candidates.value();
        } else {
            auto package_name = get_pool()->resolve_version_set_package_name(version_set_id);
            auto version_set = get_pool()->resolve_version_set(version_set_id);
            auto package_candidates = get_or_cache_candidates(package_name);
            std::vector<SolvableId> non_matching_candidates;
            for (auto p : package_candidates.candidates) {
                auto internal_solvable = get_pool()->resolve_internal_solvable(p);
                auto version = internal_solvable.get_solvable_unchecked().get_inner();
                if (!version_set.contains(version)) {
                    non_matching_candidates.push_back(p);
                }
            }
            version_set_inverse_candidates.insert(version_set_id, non_matching_candidates);
            return non_matching_candidates;
        }
    }

    // Returns the candidates for the package with the given name similar to
    // [`Self::get_or_cache_candidates`] sorted from highest to lowest.
    //
    // If the provider has requested the solving process to be cancelled, the cancellation value
    // will be returned as an `Err(...)`.
    const std::vector<SolvableId>& get_or_cache_sorted_candidates(const Requirement &requirement) {
        if (requirement.is_single()) {
            return get_or_cache_sorted_candidates_for_version_set(requirement.version_set());
        }

        if (auto candidates = requirement_to_sorted_candidates.get_ref(requirement)) {
            return *candidates;
        }

        std::vector<SolvableId> sorted_candidates;
        std::unordered_set<SolvableId> seen;
        for (const auto &version_set_id: provider.version_sets_in_union(requirement.version_set_union())) {
            const auto& candidates = get_or_cache_sorted_candidates_for_version_set(version_set_id);
            for (auto candidate : candidates) {
                if (seen.insert(candidate).second) {
                    sorted_candidates.push_back(candidate);
                }
            }
        }

        requirement_to_sorted_candidates.insert(requirement, std::move(sorted_candidates));
        return *requirement_to_sorted_candidates.get_ref(requirement);
    }

    // Returns the dependencies of a solvable. Requests the solvables from the
    // [`DependencyProvider`] if they are not known yet.
    //
    // If the provider has requested the solving process to be cancelled, the cancellation value
    // will be returned as an `Err(...)`.
    const DependenciesVariant& get_or_cache_dependencies(SolvableId solvable_id) {
        auto optional_dependencies_id = solvable_to_dependencies.get_copy(solvable_id);
        if (optional_dependencies_id.has_value()) {
            return solvable_dependencies[optional_dependencies_id.value()];
        } else {
            // Since getting the dependencies from the provider is a potentially blocking
            // operation, we want to check beforehand whether we should cancel the solving
            // process
            check_cancelled();
            auto dependencies = provider.get_dependencies(solvable_id);
            check_cancelled();
            auto dependencies_id = solvable_dependencies.alloc(dependencies);
            solvable_to_dependencies.insert_copy(solvable_id, dependencies_id);
            return solvable_dependencies[dependencies_id];
        }
    }

    // Returns true if the dependencies for the given solvable are "cheaply" available. This means
    // either the dependency provider indicated that the dependencies for a solvable are available
    // or the dependencies have already been requested.
    bool are_dependencies_available_for(const SolvableId& solvable) {
//        return false;
        auto dependencies_id = solvable_to_dependencies.get_copy(solvable);
        if (dependencies_id.has_value()) {
            return true;
        } else {
            auto solvable_idx = solvable.to_usize();
            if (hint_dependencies_available.size() > solvable_idx) {
                return hint_dependencies_available[solvable_idx];
            }
            return false;
        }
    }

private:
    void check_cancelled() {
        if (auto reason = provider.should_cancel_with_value()) {
            throw SolverCancelled(*reason);
        }
    }

    const std::vector<SolvableId>& get_or_cache_sorted_candidates_for_version_set(VersionSetId version_set_id) {
        auto requirement = requirement_single(version_set_id);
        if (auto candidates = requirement_to_sorted_candidates.get_ref(requirement)) {
            return *candidates;
        }

        auto package_name = get_pool()->resolve_version_set_package_name(version_set_id);
        auto matching_candidates = get_or_cache_matching_candidates(version_set_id);
        auto package_candidates = get_or_cache_candidates(package_name);
        std::vector<SolvableId> sorted_candidates;
        sorted_candidates.insert(sorted_candidates.end(), matching_candidates.begin(), matching_candidates.end());
        provider.sort_candidates(sorted_candidates);
        auto favored_id = package_candidates.favored;
        if (favored_id.has_value()) {
            auto pos = std::find(sorted_candidates.begin(), sorted_candidates.end(), favored_id.value());
            if (pos != sorted_candidates.end()) {
                std::rotate(sorted_candidates.begin(), pos, pos + 1);
            }
        }

        requirement_to_sorted_candidates.insert(requirement, std::move(sorted_candidates));
        return *requirement_to_sorted_candidates.get_ref(requirement);
    }

    // A mapping from package name to a list of candidates.
    Arena<CandidatesId, PackageCandidates> candidates;
    FrozenCopyMap<NameId, CandidatesId> package_name_to_candidates;
    std::map<NameId, std::shared_ptr<std::future<PackageCandidates>>> package_name_to_candidates_in_flight;

    // A mapping of `VersionSetId` to the candidates that match that set.
    FrozenMap<VersionSetId, std::vector<SolvableId>> version_set_candidates;

    // A mapping of `VersionSetId` to the candidates that do not match that set (only candidates
    // of the package indicated by the version set are included).
    FrozenMap<VersionSetId, std::vector<SolvableId>> version_set_inverse_candidates;

    // A mapping from a solvable to a list of dependencies
    Arena<DependenciesId, DependenciesVariant> solvable_dependencies;
    FrozenCopyMap<SolvableId, DependenciesId> solvable_to_dependencies;

    // A mapping that indicates that the dependencies for a particular solvable can cheaply be
    // retrieved from the dependency provider. This information is provided by the
    // DependencyProvider when the candidates for a package are requested.
    std::vector<bool> hint_dependencies_available;

//    _data: PhantomData<(VS, N)>,

};

#endif // SOLVER_CACHE_H
