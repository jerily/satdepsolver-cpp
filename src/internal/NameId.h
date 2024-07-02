#ifndef NAMEID_H
#define NAMEID_H

#include <cstddef>
#include <cstdint>
#include <cassert>
#include <memory>
#include "ArenaId.h"

// The id associated to a package name
class NameId : public ArenaId {
private:
    std::uint32_t value;

public:
    explicit NameId(std::uint32_t value) : value(value) {}

    // assignment constructor
    NameId& operator=(const NameId& other) {
        value = other.value;
        return *this;
    }

    // copy constructor
    NameId(const NameId& other) : value(other.value) {}

    std::size_t to_usize() const override {
        return static_cast<std::size_t>(value);
    }

    static NameId from_usize(std::size_t x) {
        return NameId(static_cast<std::uint32_t>(x));
    }

    bool operator==(const NameId& other) const {
        return value == other.value;
    }

    bool operator<(const NameId& other) const {
        return value < other.value;
    }
};

namespace std {
    template<>
    struct hash<NameId> {
        std::size_t operator()(const NameId& name_id) const {
            return std::hash<std::uint32_t>()(name_id.to_usize());
        }
    };
}


#endif // NAMEID_H