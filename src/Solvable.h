#ifndef SOLVABLE_H
#define SOLVABLE_H

#include <memory>
#include <string>
#include <iostream>
#include <cassert>
#include <utility>
#include <any>
#include "internal/StringId.h"
#include "internal/NameId.h"
#include "internal/VersionSetId.h"

// The Solvable class template represents a single candidate of a package
template<typename V>
class Solvable {
private:
    V inner;   // The record associated with this solvable
    NameId name; // The interned name in the Pool

public:
// Constructor
    Solvable(const V &record, const NameId& nameId) : inner(record), name(nameId) {}

    // assignment operator
    Solvable& operator=(const Solvable& other) {
        inner = other.inner;
        name = other.name;
        return *this;
    }

    // copy constructor
    Solvable(const Solvable& other) : inner(other.inner), name(other.name) {}

// Accessor for the record
    V get_inner() {
        return inner;
    }

// Accessor for the name ID
    NameId get_name_id() {
        return name;
    }
};

// The inner representation of a solvable, which can be either a package or the root solvable
namespace SolvableInner {
    struct Root {
    };

    template<typename V>
    struct Package {
        Solvable<V> solvable;
    };
}

template <typename V>
using SolvableInnerVariant = std::variant<SolvableInner::Root, SolvableInner::Package<V>>;

// The InternalSolvable class template represents a package that can be installed
template<typename V>
class InternalSolvable {
public:
    SolvableInnerVariant<V> inner;

    explicit InternalSolvable(const SolvableInnerVariant<V> &inner) : inner(inner) {}

    static InternalSolvable new_root() {
            return InternalSolvable(SolvableInner::Root{});
    }

    // new_solvable
    static InternalSolvable new_solvable(const NameId &name_id, V record) {
        return InternalSolvable(SolvableInner::Package<V>{Solvable<V>(record, name_id)});
    }

// Get the solvable if it's not root
    std::optional<Solvable<V>> get_solvable() {
        return std::visit([](auto &&arg) -> std::optional<Solvable<V>> {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, SolvableInner::Package<V>>) {
                auto package = std::any_cast<SolvableInner::Package<V>>(arg);
                return std::make_optional(package.solvable);
            } else {
                return std::nullopt;
            }
        }, inner);
    }

    // Get the solvable if it's not root
    Solvable<V> get_solvable_unchecked() {
        return get_solvable().value();
    }

};


#endif // SOLVABLE_H