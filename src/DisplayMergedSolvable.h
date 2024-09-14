#ifndef DISPLAY_MERGED_SOLVABLE_H
#define DISPLAY_MERGED_SOLVABLE_H

#include <sstream>
#include "Solvable.h"
#include "Pool.h"
#include "solver/Clause.h"

template<typename VS, typename N>
class DisplayMergedSolvable {
private:
    std::shared_ptr<Pool<VS, N>> pool;
    std::vector<SolvableId> ids;

public:
// Constructor
    explicit DisplayMergedSolvable(std::shared_ptr<Pool<VS, N>> poolRef, const std::vector<SolvableId> &idsRef)
            : pool(poolRef) {

        // copy idsRef to ids
        std::copy(idsRef.begin(), idsRef.end(), std::back_inserter(ids));
    }

    friend std::ostream &operator<<(std::ostream &os, const DisplayMergedSolvable &display_merged_solvable) {
        os << display_merged_solvable.to_string();
        return os;
    }

    std::string to_string() const {
        std::ostringstream oss;
        std::vector<typename VS::ValueType> version_records;
        std::transform(ids.begin(), ids.end(), std::back_inserter(version_records),
                                [this](const SolvableId &id) { return pool->resolve_solvable(id).get_inner(); });

        std::sort(version_records.begin(), version_records.end());

        // join the version sets with "|"
        bool first = true;
        for (const auto &vs : version_records) {
            if (!first) {
                oss << " | ";
            } else {
                first = false;
            }
            oss << vs;
        }
        return oss.str();
    }

};

#endif // DISPLAY_MERGED_SOLVABLE_H