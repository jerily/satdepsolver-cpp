#include <satdepsolver.h>
#include <solver/Solver.h>

NodeIndex other_node_id() {
    DiGraph<int, int> graph;
    return graph.add_node(2);
}
