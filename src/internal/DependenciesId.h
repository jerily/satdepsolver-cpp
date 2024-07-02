#ifndef DEPENDENCIES_ID_H
#define DEPENDENCIES_ID_H

#include "ArenaId.h"

class DependenciesId : public ArenaId {
private:
    std::uint32_t value;

public:
    explicit DependenciesId(std::uint32_t value) : value(value) {}

    // assignment constructor
    DependenciesId& operator=(const DependenciesId& other) {
        value = other.value;
        return *this;
    }

    // copy constructor
    DependenciesId(const DependenciesId& other) : value(other.value) {}

    std::size_t to_usize() const override {
        return static_cast<std::size_t>(value);
    }

    static DependenciesId from_usize(std::size_t x) {
        return DependenciesId(static_cast<std::uint32_t>(x));
    }
};

#endif // DEPENDENCIES_ID_H