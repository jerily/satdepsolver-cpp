#ifndef SATDEPSOLVER_SOLVER_CANCELLED_H
#define SATDEPSOLVER_SOLVER_CANCELLED_H

#include <stdexcept>
#include <string>

// Internal transport from provider callbacks to Solver::solve's result variant.
class SolverCancelled : public std::runtime_error {
public:
    explicit SolverCancelled(const std::string& reason) : std::runtime_error(reason) {}
};

#endif
