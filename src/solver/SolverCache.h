#ifndef SOLVER_CACHE_H
#define SOLVER_CACHE_H

#include <memory>
#include <any>
#include <future>
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

    // A mapping of `VersionSetId` to a sorted list of candidates that match that set.
    FrozenMap<VersionSetId, std::vector<SolvableId>> version_set_to_sorted_candidates;

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
            if (provider.should_cancel_with_value().has_value()) {
               throw std::runtime_error("Solver cancelled");
            }

            // Check if there is an in-flight request
            auto in_flight_request = package_name_to_candidates_in_flight.find(package_name);
            if (in_flight_request != package_name_to_candidates_in_flight.end()) {
                // Found an in-flight request, wait for that request to finish and return the computed result.
                in_flight_request->second.get();
                return candidates[package_name_to_candidates.get_copy(package_name).value()];
            } else {
                // Prepare an in-flight notifier for other requests coming in.
                package_name_to_candidates_in_flight[package_name] = std::make_shared<std::future<PackageCandidates>>();

                // Otherwise we have to get them from the DependencyProvider
                auto package_candidates = provider.get_candidates(package_name).value_or(PackageCandidates());
                // Store information about which solvables dependency information is easy to
                // retrieve.
                for (auto hint_candidate : package_candidates.hint_dependencies_available) {
                    auto idx = hint_candidate.to_usize();
                    if (hint_dependencies_available.size() <= idx) {
                        hint_dependencies_available.resize(idx + 1, false);
                    }
                    hint_dependencies_available[idx] = true;
                }

                // Allocate an ID so we can refer to the candidates from everywhere
                auto package_candidates_id = candidates.alloc(package_candidates);
                package_name_to_candidates.insert_copy(package_name, package_candidates_id);

                // Remove the in-flight request now that we inserted the result and notify any waiters
                auto notifier = package_name_to_candidates_in_flight[package_name];
                package_name_to_candidates_in_flight.erase(package_name);
// TODO: check                notifier->notify(std::numeric_limits<std::size_t>::max());

                return candidates[package_candidates_id];
            }
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
    std::vector<SolvableId> get_or_cache_sorted_candidates(VersionSetId version_set_id) {
        auto optional_candidates = version_set_to_sorted_candidates.get(version_set_id);
        if (optional_candidates.has_value()) {
            return optional_candidates.value();
        } else {
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

            version_set_to_sorted_candidates.insert(version_set_id, sorted_candidates);
            return sorted_candidates;
        }
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
            if (provider.should_cancel_with_value().has_value()) {
                fprintf(stderr, "solver cancelled");
                throw std::runtime_error("Solver cancelled");
            }

            auto dependencies = provider.get_dependencies(solvable_id);
            auto dependencies_id = solvable_dependencies.alloc(dependencies);
            solvable_to_dependencies.insert_copy(solvable_id, dependencies_id);
            return solvable_dependencies[dependencies_id];
        }
    }

    // Returns true if the dependencies for the given solvable are "cheaply" available. This means
    // either the dependency provider indicated that the dependencies for a solvable are available
    // or the dependencies have already been requested.
    bool are_dependencies_available_for(const SolvableId& solvable) {
        // TODO: check if this is correct
        return false;
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