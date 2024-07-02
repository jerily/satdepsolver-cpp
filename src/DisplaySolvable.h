#ifndef DISPLAY_SOLVABLE_H
#define DISPLAY_SOLVABLE_H

#include <sstream>
#include "Solvable.h"
#include "Pool.h"
#include "solver/Clause.h"

// The DisplaySolvable class template is used to visualize a solvable
template<typename VS, typename N>
class DisplaySolvable {
private:
    std::shared_ptr<Pool<VS, N>> pool;
    InternalSolvable<typename VS::ValueType> solvable;

public:
// Constructor
    explicit DisplaySolvable(std::shared_ptr<Pool<VS, N>> poolRef, const InternalSolvable<typename VS::ValueType> &solvableRef)
            : pool(poolRef), solvable(solvableRef) {}

    friend std::ostream &operator<<(std::ostream &os, const DisplaySolvable &display_solvable) {
        os << display_solvable.to_string();
        return os;
    }

    std::string to_string() const {
        std::ostringstream oss;
        std::visit([this, &oss](auto &&arg) {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, SolvableInner::Root>) {
                oss << "<root>";
            } else if constexpr (std::is_same_v<T, SolvableInner::Package<typename VS::ValueType>>) {
                auto package = std::any_cast<SolvableInner::Package<typename VS::ValueType>>(arg);
                oss << pool->resolve_package_name(package.solvable.get_name_id()) << "=" << package.solvable.get_inner();
            }
        }, solvable.inner);
        return oss.str();
    }

};

#endif // DISPLAY_SOLVABLE_H