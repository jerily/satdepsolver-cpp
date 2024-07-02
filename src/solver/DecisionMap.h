#ifndef DECISION_MAP_H
#define DECISION_MAP_H

#include <vector>
#include <iostream>
#include <cmath>
#include <algorithm>
#include <optional>
#include "../internal/SolvableId.h"

// Represents a decision (i.e. an assignment to a solvable) and the level at which it was made
//
// = 0: undecided
// > 0: level of decision when the solvable is set to true
// < 0: level of decision when the solvable is set to false
class DecisionAndLevel {
public:
    explicit DecisionAndLevel(int64_t value) : value_(value) {}

    static DecisionAndLevel undecided() {
        return DecisionAndLevel(0);
    }

    std::optional<bool> value() const {
        if (value_ < 0) return false;
        if (value_ > 0) return true;
        return std::nullopt;
    }

    uint32_t level() const {
        return static_cast<uint32_t>(std::abs(value_));
    }

    static DecisionAndLevel with_value_and_level(bool value, uint32_t level) {
        return DecisionAndLevel(value ? level : -static_cast<int64_t>(level));
    }

private:
    int64_t value_;
};

// A map of the assignments to solvables.
class DecisionMap {
public:
    DecisionMap() : map_() {}

    void reset(const SolvableId& solvable_id) {
        size_t index = solvable_id.to_usize();
        if (index < map_.size()) {
            map_[index] = DecisionAndLevel::undecided();
        }
    }

    void set(const SolvableId& solvable_id, bool value, uint32_t level) {
        size_t index = solvable_id.to_usize();
        if (index >= map_.size()) {
            map_.resize(index + 1, DecisionAndLevel::undecided());
        }
        map_[index] = DecisionAndLevel::with_value_and_level(value, level);
    }

    uint32_t level(const SolvableId& solvable_id) const {
        size_t index = solvable_id.to_usize();
        if (index < map_.size()) {
            return map_[index].level();
        }
        return 0;
    }

    std::optional<bool> value(const SolvableId& solvable_id) const {
        size_t index = solvable_id.to_usize();
        if (index < map_.size()) {
            return map_[index].value();
        }
        return std::nullopt;
    }

private:
    std::vector<DecisionAndLevel> map_;
};


#endif // DECISION_MAP_H