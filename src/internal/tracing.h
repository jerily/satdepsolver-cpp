#ifndef TRACING_H
#define TRACING_H

#include <cstdio>
#include <cstdarg>

namespace tracing {

#ifdef DEBUG
    void trace(const char *fmt, ...) {
        va_list args;
        va_start(args, fmt);
        vfprintf(stderr, fmt, args);
        va_end(args);
    }

    void info(const char *fmt, ...) {
        va_list args;
        va_start(args, fmt);
        vfprintf(stdout, fmt, args);
        va_end(args);
    }

    void debug(const char *fmt, ...) {
        va_list args;
        va_start(args, fmt);
        vfprintf(stderr, fmt, args);
        va_end(args);
    }
#else
    void trace(const char *fmt, ...) {}

    void info(const char *fmt, ...) {}

    void debug(const char *fmt, ...) {}
#endif

}


#endif // TRACING_H