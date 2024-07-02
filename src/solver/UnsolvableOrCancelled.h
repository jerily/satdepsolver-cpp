#ifndef UNSOLVABLEORCANCELLED_H
#define UNSOLVABLEORCANCELLED_H

#include <any>
#include "../Problem.h"

namespace UnsolvableOrCancelled {
    struct Unsolvable {
        Problem problem;
    };
    struct Cancelled {
        std::any data;
    };
}

using UnsolvableOrCancelledVariant = std::variant<UnsolvableOrCancelled::Unsolvable, UnsolvableOrCancelled::Cancelled>;

#endif // UNSOLVABLEORCANCELLED_H