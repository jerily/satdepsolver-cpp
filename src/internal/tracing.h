#ifndef TRACING_H
#define TRACING_H

#include <cstdio>
#include <cstdarg>

namespace tracing {

#ifdef DEBUG
    inline void trace(const char *fmt, ...) {
        va_list args;
        va_start(args, fmt);
        vfprintf(stderr, fmt, args);
        va_end(args);
    }

    inline void info(const char *fmt, ...) {
        va_list args;
        va_start(args, fmt);
        vfprintf(stdout, fmt, args);
        va_end(args);
    }

    inline void debug(const char *fmt, ...) {
        va_list args;
        va_start(args, fmt);
        vfprintf(stderr, fmt, args);
        va_end(args);
    }
#else
    inline void trace(const char *fmt, ...) {}

    inline void info(const char *fmt, ...) {}

    inline void debug(const char *fmt, ...) {}
#endif

}


#endif // TRACING_H