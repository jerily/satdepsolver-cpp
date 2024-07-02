#ifndef WATCHMAP_H
#define WATCHMAP_H

#include <iostream>
#include <unordered_map>
#include <vector>
#include "../internal/SolvableId.h"
#include "../internal/ClauseId.h"
#include "Clause.h"
#include "../internal/Mapping.h"

class WatchMap {
private:
    Mapping<SolvableId, ClauseId> map;

public:
    WatchMap() = default;

    Mapping<SolvableId, ClauseId> get_map() {
        return map;
    }

    void start_watching(ClauseState &clause, const ClauseId &clauseId) {
        for (size_t watch_index = 0; watch_index < clause.watched_literals_.size(); ++watch_index) {
            SolvableId watched_solvable = clause.watched_literals_[watch_index];
            ClauseId already_watching = first_clause_watching_solvable(watched_solvable);
            clause.link_to_clause(watch_index, already_watching);
            watch_solvable(watched_solvable, clauseId);
        }
    }

    void update_watched(std::optional<ClauseState> &predecessor_clause, ClauseState &clause, const ClauseId& clause_id, size_t watch_index,
                        const SolvableId &previous_watch, const SolvableId &new_watch) {
        fprintf(stderr, "................................................................ update_watched\n");
        // Remove this clause from its current place in the linked list, because we
        // are no longer watching what brought us here
        if (predecessor_clause.has_value()) {
            // Unlink the clause
            predecessor_clause.value().unlink_clause(clause, previous_watch, watch_index);
        } else {
            // This was the first clause in the chain
            map.insert(previous_watch, clause.get_linked_clause(watch_index));
        }

        // Set the new watch
        clause.watched_literals_[watch_index] = new_watch;
        auto optional_new_watch_clause_id = map.get(new_watch);
        if (!optional_new_watch_clause_id.has_value()) {
            throw std::runtime_error("linking to unknown solvable");
        }
        clause.link_to_clause(watch_index, optional_new_watch_clause_id.value());
        map.insert(new_watch, clause_id);
    }

    ClauseId first_clause_watching_solvable(const SolvableId& watched_solvable) {
        return map.get(watched_solvable).value_or(ClauseId::null());
    }

    void watch_solvable(const SolvableId& watched_solvable, const ClauseId& id) {
        map.insert(watched_solvable, id);
    }
};

#endif // WATCHMAP_H