#include <utility>

#include "../internal/ClauseId.h"
#include "../internal/SolvableId.h"

// Represents an assignment to a variable
class Decision {
public:
    SolvableId solvable_id;
    bool value;
    ClauseId derived_from;

    // Constructor
    Decision(SolvableId solvable, bool val, ClauseId derived)
            : solvable_id(std::move(solvable)), value(val), derived_from(std::move(derived)) {}
};
