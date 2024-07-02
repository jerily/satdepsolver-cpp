#ifndef ARENA_ID_H
#define ARENA_ID_H

#include <cstddef>
#include <cstdint>
#include <cassert>

class ArenaId {
public:
    virtual ~ArenaId() = default;

    virtual std::size_t to_usize() const = 0;

};

#endif // ARENA_ID_H