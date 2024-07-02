#ifndef LEARNTCLAUSEID_H
#define LEARNTCLAUSEID_H

#include "ArenaId.h"

class LearntClauseId : public ArenaId {
private:
    std::uint32_t value;

public:

    explicit LearntClauseId(std::uint32_t value) : value(value) {}

    // assignment constructor
    LearntClauseId& operator=(const LearntClauseId& other) {
        value = other.value;
        return *this;
    }

    // copy constructor
    LearntClauseId(const LearntClauseId& other) : value(other.value) {}

    std::size_t to_usize() const override {
        return static_cast<std::size_t>(value);
    }

    static LearntClauseId from_usize(std::size_t x) {
        return LearntClauseId(static_cast<std::uint32_t>(x));
    }

    bool operator==(const LearntClauseId& other) const {
        return value == other.value;
    }
};

namespace std {
    template <>
    struct hash<LearntClauseId> {
        std::size_t operator()(const LearntClauseId& id) const {
            return std::hash<std::uint32_t>{}(id.to_usize());
        }
    };
}

#endif // LEARNTCLAUSEID_H