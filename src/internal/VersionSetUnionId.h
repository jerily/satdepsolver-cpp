#ifndef VERSION_SET_UNION_ID_H
#define VERSION_SET_UNION_ID_H

#include <cstddef>
#include <cstdint>
#include <functional>
#include <sstream>
#include "ArenaId.h"

// The id associated with a union of VersionSets.
class VersionSetUnionId : public ArenaId {
private:
    std::uint32_t value;

public:
    explicit VersionSetUnionId(std::uint32_t value) : value(value) {}

    VersionSetUnionId &operator=(const VersionSetUnionId &other) {
        value = other.value;
        return *this;
    }

    VersionSetUnionId(const VersionSetUnionId &other) : value(other.value) {}

    std::size_t to_usize() const override {
        return static_cast<std::size_t>(value);
    }

    static VersionSetUnionId from_usize(std::size_t x) {
        return VersionSetUnionId(static_cast<std::uint32_t>(x));
    }

    bool operator==(const VersionSetUnionId &other) const {
        return value == other.value;
    }

    bool operator!=(const VersionSetUnionId &other) const {
        return value != other.value;
    }

    bool operator<(const VersionSetUnionId &other) const {
        return value < other.value;
    }

    std::string to_string() const {
        std::stringstream ss;
        ss << "VersionSetUnionId(" << value << ")";
        return ss.str();
    }
};

namespace std {
    template<>
    struct hash<VersionSetUnionId> {
        std::size_t operator()(const VersionSetUnionId &version_set_union_id) const {
            return std::hash<std::uint32_t>()(version_set_union_id.to_usize());
        }
    };
}

#endif // VERSION_SET_UNION_ID_H
