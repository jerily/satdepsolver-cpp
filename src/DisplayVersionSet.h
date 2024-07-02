#ifndef DISPLAY_VERSION_SET_H
#define DISPLAY_VERSION_SET_H

#include <sstream>
#include "Solvable.h"
#include "Pool.h"
#include "solver/Clause.h"
#include "DisplaySolvable.h"

// The DisplaySolvable class template is used to visualize a solvable
template<typename VS, typename N>
class DisplayVersionSet {
private:
    std::shared_ptr<Pool<VS, N>> pool;
    VS version_set;

public:
// Constructor
    explicit DisplayVersionSet(std::shared_ptr<Pool<VS, N>> poolRef, const VS &versionSetRef)
            : pool(poolRef), version_set(versionSetRef) {}

    friend std::ostream &operator<<(std::ostream &os, const DisplayVersionSet &display_version_set) {
        os << display_version_set.to_string();
        return os;
    }

    std::string to_string() const {
        std::ostringstream oss;
        oss << "VersionSet(";
        oss << version_set;
        oss << ")";
        return oss.str();
    }

};

#endif // DISPLAY_VERSION_SET_H