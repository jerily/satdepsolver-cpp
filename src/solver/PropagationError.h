#ifndef PROPAGATIONERROR_H
#define PROPAGATIONERROR_H

#include "../internal/SolvableId.h"
#include "../internal/ClauseId.h"
#include <any>


namespace PropagationError {

    struct Conflict {
        SolvableId solvable_id;
        bool is_positive{};
        ClauseId clause_id;
    };

    struct Cancelled {
        std::any data;
    };
}

using PropagationErrorVariant = std::variant<PropagationError::Conflict, PropagationError::Cancelled>;

#endif // PROPAGATIONERROR_H