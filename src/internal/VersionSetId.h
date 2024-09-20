#ifndef VERSION_SET_ID_H
#define VERSION_SET_ID_H

#include <cstddef>
#include <cstdint>
#include <cassert>
#include <functional>
#include <sstream>
#include "ArenaId.h"

// The id associated with a VersionSet.
class VersionSetId : public ArenaId {
private:
    std::uint32_t value;

public:

    explicit VersionSetId(std::uint32_t value) : value(value) {}

    // assignment constructor
    VersionSetId& operator=(const VersionSetId& other) {
        value = other.value;
        return *this;
    }

    // copy constructor
    VersionSetId(const VersionSetId& other) : value(other.value) {}

    std::size_t to_usize() const override {
        return static_cast<std::size_t>(value);
    }

    static VersionSetId from_usize(std::size_t x) {
        return VersionSetId(static_cast<std::uint32_t>(x));
    }

    size_t operator()(const VersionSetId& vsid) const {
        return std::hash<uint32_t>{}(vsid.value);
    }

    bool operator==(const VersionSetId& vsid) const {
        return value == vsid.value;
    }

    bool operator!=(const VersionSetId& vsid) const {
        return value != vsid.value;
    }

    bool operator<(const VersionSetId& vsid) const {
        return value < vsid.value;
    }

    bool operator>(const VersionSetId& vsid) const {
        return value > vsid.value;
    }

    bool operator<=(const VersionSetId& vsid) const {
        return value <= vsid.value;
    }

    bool operator>=(const VersionSetId& vsid) const {
        return value >= vsid.value;
    }

    std::string to_string() const {
        std::stringstream ss;
        ss << "VersionSetId(" << value << ")";
        return ss.str();
    }

};


namespace std {
    template<>
    struct hash<VersionSetId> {
        std::size_t operator()(const VersionSetId& version_set_id) const {
            return std::hash<std::uint32_t>()(version_set_id.to_usize());
        }
    };
}

#endif // VERSION_SET_ID_H