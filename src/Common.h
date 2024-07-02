#ifndef COMMON_H
#define COMMON_H

#include <cstddef>
#include <vector>
#include <variant>
#include <optional>
#include "internal/SolvableId.h"
#include "internal/StringId.h"
#include "Pool.h"

// A list of candidate solvables for a specific package. This is returned from
// [`DependencyProvider::get_candidates`].
struct PackageCandidates {
    // A list of all solvables for the package.
    std::vector<SolvableId> candidates;

    // Optionally the id of the solvable that is favored over other solvables. The solver will
    // first attempt to solve for the specified solvable but will fall back to other candidates if
    // no solution could be found otherwise.
    //
    // The same behavior can be achieved by sorting this candidate to the top using the
    // [`DependencyProvider::sort_candidates`] function but using this method provides better
    // error messages to the user.
    std::optional<SolvableId> favored;

    // If specified this is the Id of the only solvable that can be selected. Although it would
    // also be possible to simply return a single candidate using this field provides better error
    // messages to the user.
    std::optional<SolvableId> locked;

    // A hint to the solver that the dependencies of some of the solvables are also directly
    // available. This allows the solver to request the dependencies of these solvables
    // immediately. Having the dependency information available might make the solver much faster
    // because it has more information available up-front which provides the solver with a more
    // complete picture of the entire problem space. However, it might also be the case that the
    // solver doesnt actually need this information to form a solution. In general though, if the
    // dependencies can easily be provided one should provide them up-front.
    std::vector<SolvableId> hint_dependencies_available;

    // A list of solvables that are available but have been excluded from the solver. For example,
    // a package might be excluded from the solver because it is not compatible with the
    // runtime. The solver will not consider these solvables when forming a solution but will use
    // them in the error message if no solution could be found.
    std::vector<std::pair<SolvableId, StringId>> excluded;
};

// Holds information about the dependencies of a package when they are known.
struct KnownDependencies {
    // Defines which packages should be installed alongside the depending package and the
    // constraints applied to the package.
    std::vector<VersionSetId> requirements;

    // Defines additional constraints on packages that may or may not be part of the solution.
    // Different from `requirements`, packages in this set are not necessarily included in the
    // solution. Only when one or more packages list the package in their `requirements` is the
    // package also added to the solution.
    //
    // This is often useful to use for optional dependencies.
    std::vector<VersionSetId> constrains;
};

// Holds information about the dependencies of a package.
namespace Dependencies {
    // The dependencies are known.
    struct Known {
        KnownDependencies known_dependencies;
    };
    // The dependencies are unknown, so the parent solvable should be excluded from the solution.
    //
    // The string provides more information about why the dependencies are unknown (e.g. an error
    // message).
    struct Unknown {
        StringId reason;
    };
}

using DependenciesVariant = std::variant<Dependencies::Known, Dependencies::Unknown>;

namespace std {
    template <typename T1, typename T2>
    struct hash<std::pair<T1, T2>> {
        std::size_t operator()(const std::pair<T1, T2>& pair) const {
            auto hash1 = std::hash<T1>{}(pair.first);
            auto hash2 = std::hash<T2>{}(pair.second);
            // Combine the two hash values
            return hash1 ^ (hash2 << 1);
        }
    };
}


template<typename VS, typename N = std::string>
class DependencyProvider {
public:
    // Returns the `Pool` that is used to allocate the Ids returned from this instance
    std::shared_ptr<Pool<VS, N>> pool;
//    virtual const Pool<VS, N>& get_pool() = 0;

    // Sort the specified solvables based on which solvable to try first. The solver will
    // iteratively try to select the highest version. If a conflict is found with the highest
    // version the next version is tried. This continues until a solution is found.
    virtual void sort_candidates(std::vector<SolvableId>& solvables) = 0;

    // Obtains a list of solvables that should be considered when a package with the given name is
    // requested.
    virtual std::optional<PackageCandidates> get_candidates(NameId name) = 0;

    // Returns the dependencies for the specified solvable.
    virtual DependenciesVariant get_dependencies(SolvableId solvable) = 0;

    // Whether the solver should stop the dependency resolution algorithm.
    //
    // This method gets called at the beginning of each unit propagation round and before
    // potentially blocking operations (like [Self::get_dependencies] and [Self::get_candidates]).
    // If it returns `Some(...)`, the solver will stop and return
    // [UnsolvableOrCancelled::Cancelled].
    virtual std::optional<std::string> should_cancel_with_value() = 0;

};

#endif // COMMON_H