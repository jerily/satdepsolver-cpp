#include <satdepsolver.h>
#include <satdepsolver.h>
#include <Range.h>

NodeIndex other_node_id();
int main() {
    DiGraph<int, int> graph;
    auto first = graph.add_node(1);
    return first == 0 && other_node_id() == 0 ? 0 : 1;
}
