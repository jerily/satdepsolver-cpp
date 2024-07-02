#ifndef DISPLAY_NAME_H
#define DISPLAY_NAME_H

#include <sstream>
#include "Solvable.h"
#include "Pool.h"
#include "solver/Clause.h"

// The DisplaySolvable class template is used to visualize a solvable
template<typename VS, typename N>
class DisplayName {
private:
    std::shared_ptr<Pool<VS, N>> pool;
    NameId name_id;

public:
// Constructor
    explicit DisplayName(std::shared_ptr<Pool<VS, N>> poolRef, const NameId &nameRef)
            : pool(poolRef), name_id(nameRef) {}

    friend std::ostream &operator<<(std::ostream &os, const DisplayName &display_name) {
        os << display_name.to_string();
        return os;
    }

    std::string to_string() const {
        std::ostringstream oss;
        oss << pool->resolve_package_name(name_id);
        return oss.str();
    }

};

#endif // DISPLAY_NAME_H