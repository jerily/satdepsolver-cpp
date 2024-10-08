#ifndef PROBLEM_H
#define PROBLEM_H

#include <iostream>
#include <utility>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <variant>
#include <memory>
#include <algorithm>
#include <deque>
#include "internal/ClauseId.h"
#include "internal/SolvableId.h"

typedef uint32_t NodeIndex;
typedef uint32_t EdgeIndex;

namespace ProblemNode {
    struct Solvable {
        SolvableId solvable;
    };
    struct UnresolvedDependency {
    };
    struct Excluded {
        StringId excluded;
    };
}

using ProblemNodeVariant = std::variant<ProblemNode::Solvable, ProblemNode::UnresolvedDependency, ProblemNode::Excluded>;


namespace ConflictCause {
    // The solvable is locked
    struct Locked {
        SolvableId solvable;
    };
    // The target node is constrained by the specified version set
    struct Constrains {
        VersionSetId version;
    };
    // It is forbidden to install multiple instances of the same dependency
    struct ForbidMultipleInstances {
    };
    // The node was excluded
    struct Excluded {
    };
}
using ConflictCauseVariant = std::variant<ConflictCause::Locked, ConflictCause::Constrains, ConflictCause::ForbidMultipleInstances, ConflictCause::Excluded>;

namespace ProblemEdge {
    // The target node is a candidate for the dependency specified by the version set
    struct Requires {
        VersionSetId version_set_id;
    };
    // The target node is involved in a conflict, caused by `conflict_cause`
    struct Conflict {
        ConflictCauseVariant conflict_cause;
    };
}

using ProblemEdgeVariant = std::variant<ProblemEdge::Requires, ProblemEdge::Conflict>;

// Represents a node that has been merged with others
//
// Merging is done to simplify error messages, and happens when a group of nodes satisfies the
// following criteria:
//
// - They all have the same name
// - They all have the same predecessor nodes
// - They all have the same successor nodes
struct MergedProblemNode {
    std::vector<SolvableId> ids;
};

static uint32_t node_count = 0;

template<typename P>
class Node {
private:
    NodeIndex id;
    P payload;
public:
    explicit Node(P payload) : payload(payload), id(node_count++) {}

    P get_payload() const {
        return payload;
    }

    NodeIndex get_id() const {
        return id;
    }

    bool operator==(const Node &other) const {
        return id == other.id;
    }
};

namespace std {
    template<typename P>
    struct hash<Node < P>> {
    std::size_t operator()(const Node <P> &node) const {
        return std::hash<uint32_t>{}(node.get_id());
    }
};
}

static uint32_t edge_count = 0;

template<typename P, typename W>
class Edge {
private:
    EdgeIndex id;
    Node<P> node_from;
    Node<P> node_to;
    W weight;
public:
    Edge(Node<P> node_from, Node<P> node_to, W weight) : node_from(node_from), node_to(node_to), weight(weight),
                                                         id(edge_count++) {}

    Node<P> get_node_from() const {
        return node_from;
    }

    Node<P> get_node_to() const {
        return node_to;
    }

    W get_weight() const {
        return weight;
    }

    EdgeIndex get_id() const {
        return id;
    }

    bool operator==(const Edge &other) const {
        return id == other.id;
    }
};


namespace std {
    template<typename P, typename W>
    struct hash<Edge < P, W>> {
    std::size_t operator()(const Edge <P, W> &edge) const {
        return std::hash<uint32_t>{}(edge.get_id());
    }
};
}

template<typename P, typename W>
class DiGraph {
public:
    std::vector<Node<P>> nodes;
    std::vector<Edge<P, W>> edges;

    NodeIndex add_node(P payload) {
        Node<P> node(payload);
        nodes.push_back(node);
        return node.get_id();
    }

    void add_edge(Node<P> node_from, Node<P> node_to, W weight) {
        edges.push_back(Edge<P, W>({node_from, node_to, weight}));
    }

    uint32_t node_count() {
        return nodes.size();
    }

    std::vector<Edge<P, W>> incoming_edges(NodeIndex nodeIndex) const {
        std::vector<Edge<P, W>> incoming;
        for (const auto& edge: edges) {
            if (edge.get_node_to().get_id() == nodeIndex) {
                incoming.push_back(edge);
            }
        }
        return incoming;
    }

    std::vector<Edge<P, W>> outgoing_edges(NodeIndex nodeIndex) const {
        std::vector<Edge<P, W>> outgoing;
        for (const auto& edge: edges) {
            if (edge.get_node_from().get_id() == nodeIndex) {
                outgoing.push_back(edge);
            }
        }
        return outgoing;
    }

    std::vector<Edge<P,W>> get_edges_for_node(NodeIndex nodeIndex) const {
        std::vector<Edge<P, W>> node_edges;
        for (auto edge: edges) {
            if (edge.get_node_from().get_id() == nodeIndex || edge.get_node_to().get_id() == nodeIndex) {
                node_edges.push_back(edge);
            }
        }
        return node_edges;
    }

    void remove_node(NodeIndex node_index) {
        nodes.erase(std::remove_if(nodes.begin(), nodes.end(), [node_index](Node<P> node) {
            return node.get_id() == node_index;
        }), nodes.end());
        edges.erase(std::remove_if(edges.begin(), edges.end(), [node_index](Edge<P, W> edge) {
            return edge.get_node_from().get_id() == node_index || edge.get_node_to().get_id() == node_index;
        }), edges.end());
    }

    Node<P> get_node(NodeIndex node_index) const {
        for (auto &node: nodes) {
            if (node.get_id() == node_index) {
                return node;
            }
        }
        throw std::runtime_error("Node not found");
    }

    void print() const {
        for (auto &node: nodes) {
            std::cout << "Node: " << node.get_id() << std::endl;
        }
        for (auto &edge: edges) {
            std::cout << "Edge: " << edge.get_id() << " from: " << edge.get_node_from().get_id() << " to: " << edge.get_node_to().get_id() << std::endl;
        }
    }

    std::vector<NodeIndex> node_indices() const {
        std::vector<NodeIndex> node_indices;
        for (auto &node: nodes) {
            node_indices.push_back(node.get_id());
        }
        return node_indices;
    }
};

template<typename N, typename E>
class Bfs {
private:
    DiGraph<N, E> graph;
    NodeIndex root_node;
    std::deque<NodeIndex> queue;

    // keep track of visited nodes
    std::unordered_set<NodeIndex> visited;
public:
    Bfs(DiGraph<N, E> graph, NodeIndex root_node) : graph(graph), root_node(root_node) {
        queue.push_back(root_node);
    }

    std::optional<NodeIndex> next() {
        while (!queue.empty()) {
            auto node_index = queue.front();
            queue.pop_front();
            if (visited.find(node_index) != visited.end()) {
                continue;
            }
            for (auto &edge: graph.edges) {
                if (edge.get_node_from().get_id() == node_index) {
                    queue.push_back(edge.get_node_to().get_id());
                }
            }
            visited.insert(node_index);
            return node_index;
        }
        return std::nullopt;
    }
};

template<typename P, typename E>
class DfsPostOrder {
private:
    DiGraph<P, E> graph;
    std::vector<NodeIndex> stack;
    std::unordered_set<NodeIndex> discovered;
    std::unordered_set<NodeIndex> finished;
public:
    DfsPostOrder(DiGraph<P, E> graph, NodeIndex root_node) : graph(std::move(graph)) {
        reset(root_node);
    }

    void reset(NodeIndex root_node) {
        stack.clear();
        discovered.clear();
        finished.clear();
        stack.push_back(root_node);
    }

    // Post-order Depth-First Search (DFS) is our final tree traversal algorithm
    // that explores nodes systematically, starting from the leaves and moving towards the root.
    // The algorithm is called post-order because it visits the root after its children.

    std::optional<NodeIndex> next() {
        while (!stack.empty()) {
            auto node_index = stack.back();
            if (discovered.find(node_index) == discovered.end()) {
                discovered.insert(node_index);
                for (auto &edge: graph.outgoing_edges(node_index)) {
                    auto target = edge.get_node_to().get_id();
                    if (discovered.find(target) == discovered.end()) {
                        stack.push_back(target);
                    }
                }
            } else {
                stack.pop_back();
                if (finished.find(node_index) == finished.end()) {
                    finished.insert(node_index);
                    return node_index;
                }
            }
        }
        return std::nullopt;
    }
};

using MapKeyType = std::tuple<NameId, std::vector<NodeIndex>, std::vector<NodeIndex>>;
using MapValueType = std::vector<std::pair<NodeIndex, SolvableId>>;

// add hash function for KeyType
struct MapKeyTypeHash {
    std::size_t operator()(const MapKeyType &key) const {
        auto [name, predecessors, successors] = key;
        std::size_t hash = std::hash<NameId>{}(name);
        for (auto &predecessor: predecessors) {
            hash ^= std::hash<NodeIndex>{}(predecessor) + 0x9e3779b9 + (hash << 6) + (hash >> 2);
        }
        for (auto &successor: successors) {
            hash ^= std::hash<NodeIndex>{}(successor) + 0x9e3779b9 + (hash << 6) + (hash >> 2);
        }
        return hash;
    }
};


// Graph representation of [`Problem`]
//
// The root of the graph is the "root solvable". Note that not all the solvable's requirements are
// included in the graph, only those that are directly or indirectly involved in the conflict. See
// [`ProblemNode`] and [`ProblemEdge`] for the kinds of nodes and edges that make up the graph.
class ProblemGraph {
public:
    DiGraph<ProblemNodeVariant, ProblemEdgeVariant> graph;
    NodeIndex root_node;
    std::optional<NodeIndex> unresolved_node;

    ProblemGraph(DiGraph<ProblemNodeVariant, ProblemEdgeVariant> graph, NodeIndex root_node,
                 std::optional<NodeIndex> unresolved_node) : graph(std::move(graph)), root_node(root_node),
                                                             unresolved_node(unresolved_node) {}

    // Simplifies and collapses nodes so that these can be considered the same candidate
    template<typename VS, typename N>
    std::unordered_map<SolvableId, MergedProblemNode> simplify(std::shared_ptr<Pool<VS, N>> pool) const {
        std::unordered_map<SolvableId, MergedProblemNode> merged_candidates;

        // Gather information about nodes that can be merged
        std::unordered_map<MapKeyType, MapValueType, MapKeyTypeHash> maybe_merge;

        for (auto& node : graph.nodes) {

            //let candidate = match graph[node_id] {
            //                ProblemNode::UnresolvedDependency | ProblemNode::Excluded(_) => continue,
            //                ProblemNode::Solvable(solvable_id) => {
            //                    if solvable_id.is_root() {
            //                        continue;
            //                    } else {
            //                        solvable_id
            //                    }
            //                }
            //            };

            auto optional_candidate = std::visit([](auto &&arg) -> std::optional<SolvableId> {
                using T = std::decay_t<decltype(arg)>;
                if constexpr (std::is_same_v<T, ProblemNode::UnresolvedDependency> || std::is_same_v<T, ProblemNode::Excluded>) {
                    return std::nullopt;
                } else if constexpr (std::is_same_v<T, ProblemNode::Solvable>) {
                    auto solvable_arg = std::any_cast<ProblemNode::Solvable>(arg);
                    if (solvable_arg.solvable.is_root()) {
                        return std::nullopt;
                    } else {
                        return std::make_optional(solvable_arg.solvable);
                    }
                }
            }, node.get_payload());

            if (!optional_candidate.has_value()) {
                continue;
            }

            auto candidate = optional_candidate.value();

            //let predecessors: Vec<_> = graph
            //                .edges_directed(node_id, Direction::Incoming)
            //                .map(|e| e.source())
            //                .sorted_unstable()
            //                .collect();

            auto predecessors_edges = graph.incoming_edges(node.get_id());
            auto predecessors = std::vector<NodeIndex>();
            for (auto& edge: predecessors_edges) {
                predecessors.push_back(edge.get_node_from().get_id());
            }
            std::sort(predecessors.begin(), predecessors.end());

            //            let successors: Vec<_> = graph
            //                .edges(node_id)
            //                .map(|e| e.target())
            //                .sorted_unstable()
            //                .collect();

            auto successors_edges = graph.outgoing_edges(node.get_id());
            auto successors = std::vector<NodeIndex>();
            for (auto& edge: successors_edges) {
                successors.push_back(edge.get_node_to().get_id());
            }
            std::sort(successors.begin(), successors.end());

            //            let name = pool.resolve_solvable(candidate).name;

            auto name = pool->resolve_solvable(candidate).get_name_id();

            //            let entry = maybe_merge
            //                .entry((name, predecessors, successors))
            //                .or_insert(Vec::new());
            //
            //            entry.push((node_id, candidate));

            auto key = std::make_tuple(name, predecessors, successors);
            auto entry = maybe_merge.find(key);
            if (entry == maybe_merge.end()) {
                maybe_merge.insert(std::pair(key, std::vector<std::pair<NodeIndex, SolvableId>>()));
                entry = maybe_merge.find(key);
            }
            entry->second.push_back(std::make_pair(node.get_id(), candidate));
        }

        //let mut merged_candidates = HashMap::default();
        //        for m in maybe_merge.into_values() {
        //            if m.len() > 1 {
        //                let m = Rc::new(MergedProblemNode {
        //                    ids: m.into_iter().map(|(_, snd)| snd).collect(),
        //                });
        //                for &id in &m.ids {
        //                    merged_candidates.insert(id, m.clone());
        //                }
        //            }
        //        }



        for (const auto& [key, value] : maybe_merge) {
            auto m = MergedProblemNode();
            m.ids = std::vector<SolvableId>();

            if (value.size() > 1) {
                for (const auto& [_, snd] : value) {
                    m.ids.push_back(snd);
                }
            }

            for (const auto& id: m.ids) {
                merged_candidates.insert(std::pair(id, m));
            }
        }

        return merged_candidates;
    }

    std::unordered_set<NodeIndex> get_installable_set() const {
        std::unordered_set<NodeIndex> installable;

        // Definition: a package is installable if it does not have any outgoing conflicting edges
        // and if each of its dependencies has at least one installable option.

        DfsPostOrder<ProblemNodeVariant, ProblemEdgeVariant> dfs(graph, root_node);

        while (auto optional_node_index = dfs.next()) {
            if (unresolved_node == optional_node_index) {
                // The unresolved node isn't installable
//                std::cout << "Unresolved node isn't installable" << std::endl;
                continue;
            }
            auto node_index = optional_node_index.value();

            // Determine any incoming "exclude" edges to the node. This would indicate that the
            // node is disabled for external reasons.
            bool excluding_edges = false;
            for (auto &edge: graph.incoming_edges(node_index)) {
                if (std::holds_alternative<ProblemEdge::Conflict>(edge.get_weight())) {
                    // check if ConflictCause::Excluded
                    auto conflict_cause = std::get<ProblemEdge::Conflict>(edge.get_weight()).conflict_cause;
                    if (std::holds_alternative<ConflictCause::Excluded>(conflict_cause)) {
                        excluding_edges = true;
                        break;
                    }
                }
            }
            if (excluding_edges) {
                // Nodes with incoming disabling edges aren't installable
//                std::cout << "Nodes with incoming disabling edges aren't installable: " << node_index << std::endl;
                continue;
            }
            bool outgoing_conflicts = false;
            for (auto &edge: graph.outgoing_edges(node_index)) {
                if (std::holds_alternative<ProblemEdge::Conflict>(edge.get_weight())) {
                    outgoing_conflicts = true;
                    break;
                }
            }
            if (outgoing_conflicts) {
                // Nodes with outgoing conflicts aren't in
//                std::cout << "Nodes with outgoing conflicts aren't installable: " << node_index << std::endl;
                continue;
            }

            // Edges grouped by dependency

            //let dependencies = self
            //                .graph
            //                .edges_directed(nx, Direction::Outgoing)
            //                .map(|e| match e.weight() {
            //                    ProblemEdge::Requires(version_set_id) => (version_set_id, e.target()),
            //                    ProblemEdge::Conflict(_) => unreachable!(),
            //                })
            //                .chunk_by(|(&version_set_id, _)| version_set_id);

            std::unordered_map<VersionSetId, std::vector<NodeIndex>> dependencies;
            for (auto &edge: graph.outgoing_edges(node_index)) {
                if (std::holds_alternative<ProblemEdge::Requires>(edge.get_weight())) {

                    auto version_set_id = std::get<ProblemEdge::Requires>(edge.get_weight()).version_set_id;
                    auto target_nx = edge.get_node_to().get_id();

                    if (dependencies.find(version_set_id) == dependencies.end()) {
                        dependencies.insert(std::pair(version_set_id, std::vector<NodeIndex>()));
                    }
                    dependencies.at(version_set_id).push_back(target_nx);
                } else {
                    throw std::runtime_error("Unexpected edge type");
                }
            }

            //for (_, mut deps) in &dependencies {
            //                if deps.all(|(_, target)| !installable.contains(&target)) {
            //                    // No installable options for this dep
            //                    continue 'outer_loop;
            //                }
            //            }

            bool all_deps_installable = true;
            for (const auto &[version_set_id, deps]: dependencies) {
                bool found_installable = false;
                for (auto &target: deps) {
                    if (installable.find(target) != installable.end()) {
                        found_installable = true;
                        break;
                    }
                }
                if (!found_installable) {
                    all_deps_installable = false;
                    break;
                }
            }
            if (!all_deps_installable) {
                // No installable options for this dep
//                std::cout << "No installable options for this dep: " << node_index << std::endl;
                continue;
            }

            // The package is installable
            installable.insert(node_index);
        }

        return installable;
    }

    std::unordered_set<NodeIndex> get_missing_set() {
        std::unordered_set<NodeIndex> missing;
        if (!unresolved_node.has_value()) {
//            std::cout << "Unresolved node not found" << std::endl;
            return missing;
        }

        missing.insert(unresolved_node.value());

        DfsPostOrder<ProblemNodeVariant, ProblemEdgeVariant> dfs(graph, root_node);
        while (auto optional_node_index = dfs.next()) {
            auto node_index = optional_node_index.value();

            //let outgoing_conflicts = self
            //                .graph
            //                .edges_directed(nx, Direction::Outgoing)
            //                .any(|e| matches!(e.weight(), ProblemEdge::Conflict(_)));

            bool outgoing_conflicts = false;
            for (auto &edge: graph.outgoing_edges(node_index)) {
                if (std::holds_alternative<ProblemEdge::Conflict>(edge.get_weight())) {
                    outgoing_conflicts = true;
                    break;
                }
            }
            if (outgoing_conflicts) {
                // Nodes with outgoing conflicts aren't missing
                continue;
            }

            // Edges grouped by dependency
            std::unordered_map<VersionSetId, std::vector<NodeIndex>> dependencies;
            for (auto &edge: graph.outgoing_edges(node_index)) {
                if (std::holds_alternative<ProblemEdge::Requires>(edge.get_weight())) {
                    auto version_set_id = std::get<ProblemEdge::Requires>(edge.get_weight()).version_set_id;
                    auto target_nx = edge.get_node_to().get_id();
                    if (dependencies.find(version_set_id) == dependencies.end()) {
                        dependencies.insert(std::pair(version_set_id, std::vector<NodeIndex>()));
                    }
                    dependencies.at(version_set_id).push_back(target_nx);
                }
            }

            // Missing if at least one dependency is missing
            //if dependencies
            //                .into_iter()
            //                .any(|(_, mut deps)| deps.all(|(_, target)| missing.contains(&target)))

            bool found_missing = false;
            for (auto &[version_set_id, deps]: dependencies) {

                bool all_deps_missing = true;
                for (auto &target: deps) {
                    if (missing.find(target) == missing.end()) {
                        all_deps_missing = false;
                        break;
                    }
                }

                if (all_deps_missing) {
                    found_missing = true;
                    break;
                }
            }

            if (found_missing) {
                missing.insert(node_index);
            }
        }
        return missing;
    }

    template<typename VS, typename N>
    std::string graphviz(std::shared_ptr<Pool<VS, N>> pool, bool do_simplify) const {

        std::stringstream ss;

        //let merged_nodes = if simplify {
        //            self.simplify(pool)
        //        } else {
        //            HashMap::default()
        //        };

        auto merged_nodes = do_simplify ? simplify(pool) : std::unordered_map<SolvableId, MergedProblemNode>();

        ss << "digraph {";

        for(const auto nx : graph.node_indices()) {
            //let id = match graph.node_weight(nx).as_ref().unwrap() {
            //                ProblemNode::Solvable(id) => *id,
            //                _ => continue,
            //            };

            const auto &node = graph.get_node(nx);
            auto optional_id = std::visit([](const auto &&arg) -> std::optional<SolvableId> {
                using T = std::decay_t<decltype(arg)>;
                if constexpr (std::is_same_v<T, ProblemNode::Solvable>) {
                    auto T_arg = std::any_cast<ProblemNode::Solvable>(arg);
                    return T_arg.solvable;
                } else {
                    return std::nullopt;
                }
            }, node.get_payload());

            if (!optional_id.has_value()) {
                continue;
            }

            auto id = optional_id.value();

            // If this is a merged node, skip it unless it is the first one in the group

            //            if let Some(merged) = merged_nodes.get(&id) {
            //                if id != merged.ids[0] {
            //                    continue;
            //                }
            //            }

            if (merged_nodes.find(id) != merged_nodes.end()) {
                auto merged = merged_nodes.at(id);
                if (id != merged.ids[0]) {
                    continue;
                }
            }

            auto solvable = pool->resolve_internal_solvable(id);
            std::unordered_set<SolvableId> added_edges;
            for (const auto& edge : graph.outgoing_edges(nx)) {
                auto target_payload = edge.get_node_to().get_payload();

                //let color = match edge.weight() {
                //                    ProblemEdge::Requires(_) if target != ProblemNode::UnresolvedDependency => {
                //                        "black"
                //                    }
                //                    _ => "red",
                //                };

                std::string color;
                if (std::holds_alternative<ProblemEdge::Requires>(edge.get_weight()) && !std::holds_alternative<ProblemNode::UnresolvedDependency>(target_payload)) {
                    color = "black";
                } else {
                    color = "red";
                }

                //let label = match edge.weight() {
                //                    ProblemEdge::Requires(version_set_id)
                //                    | ProblemEdge::Conflict(ConflictCause::Constrains(version_set_id)) => {
                //                        pool.resolve_version_set(*version_set_id).to_string()
                //                    }
                //                    ProblemEdge::Conflict(ConflictCause::ForbidMultipleInstances)
                //                    | ProblemEdge::Conflict(ConflictCause::Locked(_)) => {
                //                        "already installed".to_string()
                //                    }
                //                    ProblemEdge::Conflict(ConflictCause::Excluded) => "excluded".to_string(),
                //                };

                std::string label = std::visit([&pool](auto &&arg) -> std::string {
                    using T = std::decay_t<decltype(arg)>;
                    if constexpr (std::is_same_v<T, ProblemEdge::Requires>) {
                        auto T_arg = std::any_cast<ProblemEdge::Requires>(arg);
                        auto display_vs = DisplayVersionSet<VS, N>(pool, pool->resolve_version_set(T_arg.version_set_id));
                        return display_vs.to_string();
                    } else if constexpr (std::is_same_v<T, ProblemEdge::Conflict>) {
                        auto T_arg = std::any_cast<ProblemEdge::Conflict>(arg);
                        auto conflict_cause = T_arg.conflict_cause;
                        if (std::holds_alternative<ConflictCause::Constrains>(conflict_cause)) {
                            auto version_set_id = std::get<ConflictCause::Constrains>(conflict_cause).version;
                            auto display_vs = DisplayVersionSet<VS, N>(pool, pool->resolve_version_set(version_set_id));
                            return display_vs.to_string();
                        } else {
                            return "already installed";
                        }
                    } else if constexpr (std::is_same_v<T, ProblemEdge::Conflict>) {
                        return "excluded";
                    }
                    return "unreachable";
                }, edge.get_weight());

                //let target = match target {
                //                    ProblemNode::Solvable(mut solvable_2) => {
                //                        // If the target node has been merged, replace it by the first id in the group
                //                        if let Some(merged) = merged_nodes.get(&solvable_2) {
                //                            solvable_2 = merged.ids[0];
                //
                //                            // Skip the edge if we would be adding a duplicate
                //                            if !added_edges.insert(solvable_2) {
                //                                continue;
                //                            }
                //                        }
                //
                //                        solvable_2.display(pool).to_string()
                //                    }
                //                    ProblemNode::UnresolvedDependency => "unresolved".to_string(),
                //                    ProblemNode::Excluded(reason) => {
                //                        format!("reason: {}", pool.resolve_string(reason))
                //                    }
                //                };

                std::optional<std::string> optional_target = std::visit([&pool, &merged_nodes, &added_edges](auto &&arg) -> std::optional<std::string> {
                    using T = std::decay_t<decltype(arg)>;
                    if constexpr (std::is_same_v<T, ProblemNode::Solvable>) {
                        auto T_arg = std::any_cast<ProblemNode::Solvable>(arg);
                        auto solvable_2 = T_arg.solvable;
                        if (merged_nodes.find(solvable_2) != merged_nodes.end()) {
                            auto merged = merged_nodes.at(solvable_2);
                            solvable_2 = merged.ids[0];
                            if (!added_edges.insert(solvable_2).second) {
                                return std::nullopt;
                            }
                        }
                        auto display_solvable = DisplaySolvable<VS,N>(pool, pool->resolve_internal_solvable(solvable_2));
                        return display_solvable.to_string();
                    } else if constexpr (std::is_same_v<T, ProblemNode::UnresolvedDependency>) {
                        return "unresolved";
                    } else if constexpr (std::is_same_v<T, ProblemNode::Excluded>) {
                        auto T_arg = std::any_cast<ProblemNode::Excluded>(arg);
                        auto reason = T_arg.excluded;
                        return "reason: " + pool->resolve_string(reason);
                    }
                    return "unreachable";
                }, target_payload);

                if (!optional_target.has_value()) {
                    continue;
                }

                auto target = optional_target.value();

                //write!(
                //                    f,
                //                    "\"{}\" -> \"{}\"[color={color}, label=\"{label}\"];",
                //                    solvable.display(pool),
                //                    target
                //                )?;

                auto display_solvable = DisplaySolvable<VS,N>(pool, solvable);
                ss << "\"" << display_solvable.to_string() << "\" -> \"" << target << "\"[color=" << color << ", label=\"" << label << "\"];";

            }
        }

        ss << "}" << std::endl;

        return ss.str();
    }
};


enum ChildOrder {
    HasRemainingSiblings,
    Last,
};

class Indenter {
private:
    std::vector<ChildOrder> levels;
    bool top_level_indent;
public:
    explicit Indenter(bool top_level_indent) : top_level_indent(top_level_indent) {
    }

    Indenter(bool top_level_indent, std::vector<ChildOrder> levels) : top_level_indent(top_level_indent),
                                                                      levels(std::move(levels)) {
    }

    bool is_at_top_level() {
        return levels.size() == 1;
    }

    Indenter push_level() {
        return push_level_with_order(ChildOrder::HasRemainingSiblings);
    }

    Indenter push_level_with_order(ChildOrder order) {
        std::vector<ChildOrder> new_levels;
        new_levels.reserve(levels.size());
        for (auto level : levels) {
            new_levels.push_back(level);
        }
        new_levels.push_back(order);
        return {top_level_indent, new_levels};
    }

    void set_last() {
        levels.back() = ChildOrder::Last;
    }

    std::string get_indent() {
        assert(!levels.empty());

        std::string s;

        auto deepest_level = levels.size() - 1;

        for (auto level = 0; level < levels.size(); level++) {
            if (level == 0 && !top_level_indent) {
                continue;
            }

            auto is_at_deepest_level = level == deepest_level;

            std::string tree_prefix;
            if (is_at_deepest_level) {
                if (levels[level] == ChildOrder::HasRemainingSiblings) {
                    tree_prefix = "├─";
                } else {
                    tree_prefix = "└─";
                }
            } else {
                if (levels[level] == ChildOrder::HasRemainingSiblings) {
                    tree_prefix = "│ ";
                } else {
                    tree_prefix = "  ";
                }
            }

            s += tree_prefix;
            s += ' ';
        }

        return s;
    }
};

class Problem {
public:
    std::vector<ClauseId> clauses;

    Problem() = default;

    void add_clause(const ClauseId &clauseId) {
        if (std::find(clauses.cbegin(), clauses.cend(), clauseId) == clauses.cend()) {
            clauses.push_back(clauseId);
        }
    }

    NodeIndex
    add_node(DiGraph<ProblemNodeVariant, ProblemEdgeVariant> &graph, std::unordered_map<SolvableId, NodeIndex> &nodes,
             const SolvableId& solvable_id) {
        if (nodes.find(solvable_id) == nodes.end()) {
            auto node_index = graph.add_node(ProblemNode::Solvable{solvable_id});
            nodes.insert(std::pair(solvable_id, node_index));
        }
        return nodes.at(solvable_id);
    }

};


void reset_problem_count() {
    node_count = 0;
    edge_count = 0;
}

#endif // PROBLEM_H