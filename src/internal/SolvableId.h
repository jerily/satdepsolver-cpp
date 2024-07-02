#ifndef SOLVABLE_ID_H
#define SOLVABLE_ID_H

#include <cstddef>
#include <cstdint>
#include <cassert>
#include <iostream>
#include <limits>
#include "ArenaId.h"

class SolvableId: ArenaId {
public:

    explicit SolvableId(uint32_t id) : value_(id) {}

    // assignment constructor
    SolvableId& operator=(const SolvableId& other) {
        value_ = other.value_;
        return *this;
    }

    // copy constructor
    SolvableId(const SolvableId& other) : value_(other.value_) {}

    static SolvableId root() {
        return SolvableId(0);
    }

    bool is_root() const {
        return value_ == 0;
    }

    static SolvableId null() {
        return SolvableId(std::numeric_limits<uint32_t>::max());
    }

    bool is_null() const {
        return value_ == std::numeric_limits<uint32_t>::max();
    }

    bool operator==(const SolvableId& other) const {
        return value_ == other.value_;
    }

    bool operator!=(const SolvableId& other) const {
        return value_ != other.value_;
    }

    bool operator==(uint32_t other) const {
        return value_ == other;
    }

    bool operator!=(uint32_t other) const {
        return value_ != other;
    }

    bool operator!() const {
        return !value_;
    }

    std::size_t to_usize() const override {
        return static_cast<std::size_t>(value_);
    }

    static SolvableId from_usize(std::size_t x) {
        return SolvableId(static_cast<std::uint32_t>(x));
    }

private:
    uint32_t value_;
};


namespace std {
    template<>
    struct hash<SolvableId> {
        std::size_t operator()(const SolvableId& solvable_id) const {
            return std::hash<std::uint32_t>()(solvable_id.to_usize());
        }
    };
}


#endif // SOLVABLE_ID_H