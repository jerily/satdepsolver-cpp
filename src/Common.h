#ifndef COMMON_H
#define COMMON_H

#include <cstddef>
#include <cstdint>
#include <sstream>
#include <stdexcept>
#include <vector>
#include <variant>
#include <optional>
#include "internal/SolvableId.h"
#include "internal/StringId.h"
#include "internal/VersionSetUnionId.h"
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

class Requirement {
public:
    enum class Kind : std::uint8_t {
        Single,
        Union,
    };

private:
    Kind kind_;
    std::uint32_t value_;

public:
    Requirement(const VersionSetId &version_set_id)
            : kind_(Kind::Single), value_(static_cast<std::uint32_t>(version_set_id.to_usize())) {}
    Requirement(const VersionSetUnionId &version_set_union_id)
            : kind_(Kind::Union), value_(static_cast<std::uint32_t>(version_set_union_id.to_usize())) {}
    Requirement(const Requirement &) = default;
    Requirement &operator=(const Requirement &) = default;

    Kind kind() const {
        return kind_;
    }

    bool is_single() const {
        return kind_ == Kind::Single;
    }

    bool is_union() const {
        return kind_ == Kind::Union;
    }

    VersionSetId version_set() const {
        if (!is_single()) {
            throw std::runtime_error("Requirement does not contain a single version set");
        }
        return VersionSetId(value_);
    }

    VersionSetUnionId version_set_union() const {
        if (!is_union()) {
            throw std::runtime_error("Requirement does not contain a version set union");
        }
        return VersionSetUnionId(value_);
    }

    template<typename VS, typename N>
    std::vector<VersionSetId> version_sets(const std::shared_ptr<Pool<VS, N>> &pool) const {
        if (is_single()) {
            return {version_set()};
        }

        return pool->resolve_version_set_union(version_set_union());
    }

    bool operator==(const Requirement &other) const {
        return kind_ == other.kind_ && value_ == other.value_;
    }

    bool operator!=(const Requirement &other) const {
        return !(*this == other);
    }

    bool operator<(const Requirement &other) const {
        if (kind_ != other.kind_) {
            return kind_ < other.kind_;
        }
        return value_ < other.value_;
    }
};

inline Requirement requirement_single(const VersionSetId &version_set_id) {
    return Requirement(version_set_id);
}

inline Requirement requirement_union(const VersionSetUnionId &version_set_union_id) {
    return Requirement(version_set_union_id);
}

template<typename VS, typename N>
std::string display_requirement(const std::shared_ptr<Pool<VS, N>> &pool, const Requirement &requirement) {
    std::ostringstream oss;
    bool first = true;
    for (const auto &version_set_id: requirement.version_sets(pool)) {
        if (!first) {
            oss << " | ";
        }
        first = false;

        auto package_name = pool->resolve_package_name(pool->resolve_version_set_package_name(version_set_id));
        oss << package_name;

        std::ostringstream version_set_stream;
        version_set_stream << pool->resolve_version_set(version_set_id);
        auto version_set_text = version_set_stream.str();
        if (!version_set_text.empty()) {
            oss << " " << version_set_text;
        }
    }

    return oss.str();
}

// Holds information about the dependencies of a package when they are known.
struct KnownDependencies {
    // Defines which packages should be installed alongside the depending package and the
    // constraints applied to the package.
    std::vector<Requirement> requirements;

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

    template <>
    struct hash<Requirement> {
        std::size_t operator()(const Requirement &requirement) const {
            auto kind_hash = std::hash<std::uint8_t>{}(static_cast<std::uint8_t>(requirement.kind()));
            auto value_hash = requirement.is_single()
                              ? std::hash<VersionSetId>{}(requirement.version_set())
                              : std::hash<VersionSetUnionId>{}(requirement.version_set_union());
            return kind_hash ^ (value_hash << 1);
        }
    };
}


template<typename VS, typename N = std::string>
class DependencyProvider {
public:
    virtual ~DependencyProvider() = default;

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

    // Returns the version sets that make up the specified version set union.
    virtual std::vector<VersionSetId> version_sets_in_union(VersionSetUnionId version_set_union_id) {
        return pool->resolve_version_set_union(version_set_union_id);
    }

};

class Problem {
private:
    std::vector<Requirement> requirements_;
    std::vector<VersionSetId> constraints_;
    std::vector<SolvableId> soft_requirements_;

public:
    Problem() = default;

    Problem &requirements(std::vector<Requirement> requirements) {
        requirements_ = std::move(requirements);
        return *this;
    }

    Problem &requirements(const std::vector<VersionSetId> &requirements) {
        requirements_.clear();
        requirements_.reserve(requirements.size());
        for (const auto &requirement: requirements) {
            requirements_.push_back(requirement_single(requirement));
        }
        return *this;
    }

    const std::vector<Requirement> &requirements() const {
        return requirements_;
    }

    Problem &constraints(std::vector<VersionSetId> constraints) {
        constraints_ = std::move(constraints);
        return *this;
    }

    const std::vector<VersionSetId> &constraints() const {
        return constraints_;
    }

    Problem &soft_requirements(std::vector<SolvableId> soft_requirements) {
        soft_requirements_ = std::move(soft_requirements);
        return *this;
    }

    const std::vector<SolvableId> &soft_requirements() const {
        return soft_requirements_;
    }
};

#endif // COMMON_H
