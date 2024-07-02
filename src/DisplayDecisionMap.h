#ifndef DISPLAY_DECISION_MAP_H
#define DISPLAY_DECISION_MAP_H

#include <sstream>
#include <utility>
#include "Solvable.h"
#include "Pool.h"
#include "solver/Clause.h"
#include "solver/DecisionMap.h"
#include "DisplaySolvable.h"

// The DisplaySolvable class template is used to visualize a solvable
template<typename VS, typename N>
class DisplayDecisionMap {
private:
    std::shared_ptr<Pool<VS, N>> pool;
    DecisionMap map;

public:
// Constructor
    explicit DisplayDecisionMap(std::shared_ptr<Pool<VS, N>> poolRef, DecisionMap decisionMap)
            : pool(poolRef), map(std::move(decisionMap)) {}

    friend std::ostream &operator<<(std::ostream &os, const DisplayDecisionMap &display_decision_map) {
        os << display_decision_map.to_string();
        return os;
    }

    std::string to_string() const {
        std::ostringstream oss;
        auto it = pool->solvables.iter();
        while (it.has_next()) {
            auto [id, solvable] = it.next().value();
            auto display_solvable = DisplaySolvable<VS, N>(pool, pool->resolve_internal_solvable(id));
            oss << display_solvable.to_string() << " := ";
            auto optional_value = map.value(id);
            if (optional_value.has_value()) {
                oss << (optional_value.value() ? "true " : "false");
                oss << " (level: " << map.level(id) << ")" << std::endl;
            } else {
                oss << "undecided" << std::endl;
            }
        }
        return oss.str();
    }

};

#endif // DISPLAY_DECISION_MAP_H