#ifndef TRACING_H
#define TRACING_H

#include <cstdio>
#include <cstdarg>

namespace tracing {

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


}


#endif // TRACING_H