#ifndef POOL_H
#define POOL_H

#include <unordered_map>
#include <vector>
#include <string>
#include <memory>
#include <iostream>
#include <cassert>
#include <optional>
#include "internal/StringId.h"
#include "internal/NameId.h"
#include "internal/SolvableId.h"
#include "internal/VersionSetId.h"
#include "internal/ClauseId.h"
#include "internal/Arena.h"
#include "internal/FrozenCopyMap.h"
#include "Solvable.h"

// A pool that stores data related to the available packages.
//
// A pool never releases its memory until it is dropped. References returned by the pool will
// remain valid for the lifetime of the pool. This allows inserting into the pool without requiring
// a mutable reference to the pool.
// This is what we refer to as `Frozen` data can be added but old data can never be removed or mutated.
template<typename VS, typename N = std::string>
class Pool {
private:
    // Interned package names
    Arena<NameId, N> package_names;
    // Interned strings
    Arena<StringId, std::string> strings;
    // Map from version set to the id of their interned counterpart
    FrozenCopyMap<std::pair<NameId, VS>, VersionSetId> version_set_to_id;

public:
    // All the solvables that have been registered
    Arena<SolvableId, InternalSolvable<typename VS::ValueType>> solvables;

    // Map from package names to the id of their interned counterpart
    std::unordered_map<N, NameId> names_to_ids;

    // Map from package names to the id of their interned counterpart
    std::unordered_map<std::string, StringId> string_to_ids;

    // Interned match specs
    Arena<VersionSetId, std::pair<NameId, VS>> version_sets;

    Pool() {
// Initialize with a root solvable if needed
        solvables.alloc(InternalSolvable<typename VS::ValueType>::new_root());
    }

    static Pool default_pool() {
        return Pool();
    }

    // Interns a generic string into the `Pool` and returns its `StringId`. Strings are
    // deduplicated.
    StringId intern_string(const std::string &name) {
        auto it = string_to_ids.find(name);
        if (it != string_to_ids.end()) {
            return it->second;
        }

        StringId id = strings.alloc(name);
        string_to_ids.insert(std::make_pair(name, id));
        return id;
    }

    // Returns the string associated with the provided [`StringId`].
    //
    // Panics if the string is not found in the pool.
    const std::string &resolve_string(const StringId &string_id) const {
        return strings[string_id];
    }

    // Interns a package name into the `Pool`, returning its `NameId`. Names are deduplicated. If
    // the same name is inserted twice the same `NameId` will be returned.
    //
    // The original name can be resolved using the [`Self::resolve_package_name`] function.
    NameId intern_package_name(const N &name) {
        auto it = names_to_ids.find(name);
        if (it != names_to_ids.end()) {
            return it->second;
        }

        NameId next_id = package_names.alloc(name);
        names_to_ids.insert(std::make_pair(name, next_id));
        return next_id;
    }

    // Returns the package name associated with the provided [`NameId`].
    //
    // Panics if the package name is not found in the pool.
    N resolve_package_name(const NameId &name_id) {
        return package_names[name_id];
    }

    // Returns the [`NameId`] associated with the specified name or `None` if the name has not
    // previously been interned using [`Self::intern_package_name`].
    std::optional<NameId> lookup_package_name(const N &name) const {
        auto it = names_to_ids.find(name);
        if (it != names_to_ids.end()) {
            return it->second;
        }
        return std::nullopt;
    }

    // Adds a solvable to a repo and returns it's [`SolvableId`].
    //
    // Unlike some of the other interning functions this function does *not* deduplicate any of the
    // inserted elements. A unique Id will be returned everytime this function is called.
    SolvableId intern_solvable(NameId name_id, const typename VS::ValueType &record) {
        return solvables.alloc(InternalSolvable<typename VS::ValueType>::new_solvable(name_id, record));
    }

    // Returns the solvable associated to the provided id
    //
    // Panics if the solvable is not found in the pool
    Solvable<typename VS::ValueType> resolve_solvable(const SolvableId& id) {
        return resolve_internal_solvable(id).get_solvable_unchecked();
    }

    // Returns the solvable associated to the provided id
    //
    // Panics if the solvable is not found in the pool
    InternalSolvable<typename VS::ValueType> resolve_internal_solvable(const SolvableId& id) {
        return solvables[id];
    }

    // Interns a version set into the [`Pool`], returning its [`VersionSetId`]. The returned
    // [`VersionSetId`] can be used to retrieve a reference to the original version set using
    // [`Self::resolve_version-set`].
    //
    // A version set is always associated with a specific package name to which it applies. The
    // passed in package name can be retrieved using [`Self::resolve_version_set_package_name`].
    //
    // Version sets are deduplicated. This means that if the same version set is inserted twice
    // they will share the same [`VersionSetId`].
    VersionSetId intern_version_set(const NameId &package_name, const VS &version_set) {
        auto optional_id = version_set_to_id.get_copy(std::make_pair<>(package_name, version_set));
        if (optional_id.has_value()) {
            return optional_id.value();
        } else {
            auto id = version_sets.alloc(std::make_pair<>(package_name, version_set.clone()));
            version_set_to_id.insert_copy(std::make_pair<>(package_name, version_set), id);
            return id;
        }
    }

    // Returns the version set associated with the provided id
    //
    // Panics if the version set is not found in the pool
    const VS &resolve_version_set(VersionSetId id) const {
        return version_sets[id].second;
    }

    // Returns the package name associated with the provide id.
    //
    // Panics if the version set is not found in the pool
    NameId resolve_version_set_package_name(VersionSetId id) const {
        return version_sets[id].first;
    }
};

// The NameDisplay class needs to be defined
template<typename VS, typename N>
class NameDisplay {
private:
    NameId id;
    const Pool<VS, N> &pool;

public:
    NameDisplay(NameId id, const Pool<VS, N> &pool) : id(id), pool(pool) {}

    void display() const {
        std::cout << pool.resolve_package_name(id);
    }
};

#endif // POOL_H