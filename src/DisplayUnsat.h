#ifndef DISPLAY_UNSAT_H
#define DISPLAY_UNSAT_H

#include <sstream>
#include <utility>
#include "Pool.h"
#include "DisplaySolvable.h"
#include "DisplayMergedSolvable.h"
#include "Problem.h"

namespace DisplayOp {
    struct Requirement {
        VersionSetId version_set_id;
        std::vector<EdgeIndex> edges;
    };
    struct Candidate {
        NodeIndex node_index;
    };
}

using DisplayOpVariant = std::variant<DisplayOp::Requirement, DisplayOp::Candidate>;

template<typename VS, typename N>
class DisplayUnsat {
private:
    std::shared_ptr<Pool<VS, N>> pool;
    ProblemGraph graph;
    std::unordered_map<SolvableId, MergedProblemNode> merged_candidates;
    std::unordered_set<NodeIndex> installable_set;
    std::unordered_set<NodeIndex> missing_set;

public:
// Constructor
    explicit DisplayUnsat(std::shared_ptr<Pool<VS, N>> poolRef, ProblemGraph graphRef)
            : pool(poolRef), graph(std::move(graphRef)) {
        merged_candidates = graph.simplify(pool);
        installable_set = graph.get_installable_set();
        missing_set = graph.get_missing_set();

        /*
        for (auto installable : installable_set) {
            auto node = graph.graph.get_node(installable);
            auto payload = node.get_payload();
            if (std::holds_alternative<ProblemNode::Solvable>(payload)) {
                auto solvable_id = std::get<ProblemNode::Solvable>(payload).solvable;
                auto internal_solvable = pool->resolve_internal_solvable(solvable_id);
                auto display_solvable = DisplaySolvable<VS, N>(pool, internal_solvable);
                std::cout << "here: " << display_solvable.to_string() << std::endl;
            }
        }


        DfsPostOrder<ProblemNodeVariant, ProblemEdgeVariant> dfs(graph.graph, graph.root_node);
        while (auto optional_node_index = dfs.next()) {
            auto node_index = optional_node_index.value();
            auto node = graph.graph.get_node(node_index);
            auto payload = node.get_payload();
            if (std::holds_alternative<ProblemNode::Solvable>(payload)) {
                auto solvable_id = std::get<ProblemNode::Solvable>(payload).solvable;
                auto internal_solvable = pool->resolve_internal_solvable(solvable_id);
                auto display_solvable = DisplaySolvable<VS, N>(pool, internal_solvable);
                std::cout << "dfs-here: " << node_index << ": " << display_solvable.to_string() << std::endl;
            }
        }
        */
    }

    friend std::ostream &operator<<(std::ostream &os, const DisplayUnsat &display_unsat) {
        os << display_unsat.to_string();
        return os;
    }

    // toString method
    std::string to_string() {
        std::ostringstream oss;

        //let (top_level_missing, top_level_conflicts): (Vec<_>, _) = self
        //            .graph
        //            .graph
        //            .edges(self.graph.root_node)
        //            .partition(|e| self.missing_set.contains(&e.target()));

        auto root_edges = graph.graph.outgoing_edges(graph.root_node);

        std::vector<Edge<ProblemNodeVariant, ProblemEdgeVariant>> top_level_missing;
        std::copy_if(root_edges.begin(), root_edges.end(), std::back_inserter(top_level_missing), [this](auto &edge) -> bool {
            return missing_set.find(edge.get_node_to().get_id()) != missing_set.end();
        });

        std::vector<Edge<ProblemNodeVariant, ProblemEdgeVariant>> top_level_conflicts;
        std::copy_if(root_edges.begin(), root_edges.end(), std::back_inserter(top_level_conflicts), [this](auto &edge) -> bool {
            return missing_set.find(edge.get_node_to().get_id()) == missing_set.end();
        });

        if (!top_level_missing.empty()) {
            fmt_graph(oss, top_level_missing, false);
        }

        if (!top_level_conflicts.empty()) {
            oss << "The following packages are incompatible" << std::endl;
            try {
                fmt_graph(oss, top_level_conflicts, true);
            } catch (std::exception &e) {
                oss << "Error: " << e.what() << std::endl;
            }

            // Conflicts caused by locked dependencies
            auto edges = graph.graph.outgoing_edges(graph.root_node);
            auto indenter = Indenter(true);

            for (auto it = edges.begin(); it != edges.end(); ++it) {
                const auto& edge = *it;

                //                let conflict = match e.weight() {
                //                    ProblemEdge::Requires(_) => continue,
                //                    ProblemEdge::Conflict(conflict) => conflict,
                //                };

                if (!std::holds_alternative<ProblemEdge::Conflict>(edge.get_weight())) {
                    continue;
                }

                auto peek_next = std::next(it);
                auto child_order = std::holds_alternative<ProblemEdge::Conflict>(peek_next->get_weight()) ? ChildOrder::HasRemainingSiblings : ChildOrder::Last;
                indenter = indenter.push_level_with_order(child_order);
                auto indent = indenter.get_indent();

                auto conflict = std::get<ProblemEdge::Conflict>(edge.get_weight()).conflict_cause;

                if (std::holds_alternative<ConflictCause::Constrains>(conflict) || std::holds_alternative<ConflictCause::ForbidMultipleInstances>(conflict)) {
                    throw std::runtime_error("Unreachable");
                } else if (std::holds_alternative<ConflictCause::Locked>(conflict)) {
                    auto locked_solvable = std::get<ConflictCause::Locked>(conflict).solvable;
                    auto locked = pool->resolve_solvable(locked_solvable);
                    auto display_name = DisplayName<VS, N>(pool, locked.get_name_id());
                    auto display_merged_solvable = DisplayMergedSolvable<VS, N>(pool, {locked_solvable});
                    oss << indent << display_name.to_string() << " " << display_merged_solvable.to_string() << " is locked, but another version is required as reported above" << std::endl;
                } else if (std::holds_alternative<ConflictCause::Excluded>(conflict)) {
                    continue;
                }
            }
        }

        return oss.str();
    }

    std::string fmt_graph(std::ostringstream& oss, std::vector<Edge<ProblemNodeVariant, ProblemEdgeVariant>> top_level_edges, bool top_level_indent) const {

        auto reported = std::unordered_set<SolvableId>();
        // Note: we are only interested in requires edges here
        auto indenter = Indenter(top_level_indent);

        // .filter(|e| e.weight().try_requires().is_some())

        std::vector<Edge<ProblemNodeVariant, ProblemEdgeVariant>> requires_edges;
        std::copy_if(top_level_edges.begin(), top_level_edges.end(), std::back_inserter(requires_edges), [](auto edge) {
            return std::holds_alternative<ProblemEdge::Requires>(edge.get_weight());
        });

        //            .chunk_by(|e| e.weight().requires())
        //            .into_iter()
        //            .map(|(version_set_id, group)| {
        //                let edges: Vec<_> = group.map(|e| e.id()).collect();
        //                (version_set_id, edges)
        //            })

        std::unordered_map<VersionSetId, std::vector<EdgeIndex>> chunked;
        for (const auto& edge : requires_edges) {
            auto requires = std::get<ProblemEdge::Requires>(edge.get_weight());
            if (chunked.find(requires.version_set_id) == chunked.end()) {
                chunked[requires.version_set_id] = std::vector<EdgeIndex>();
            }
            chunked.at(requires.version_set_id).push_back(edge.get_id());
        }

        //.sorted_by_key(|(_version_set_id, edges)| {
        //                edges
        //                    .iter()
        //                    .any(|&edge| installable_nodes.contains(&graph.edge_endpoints(edge).unwrap().1))
        //            })

        std::vector<std::pair<VersionSetId, std::vector<EdgeIndex>>> chunked_vector(chunked.begin(), chunked.end());
        std::sort(chunked_vector.begin(), chunked_vector.end(), [this](auto &a, auto &b) -> bool {
            auto a_edges = a.second;
            auto b_edges = b.second;
            auto a_installable = std::any_of(a_edges.begin(), a_edges.end(), [this](auto &edge) {
                return installable_set.find(graph.graph.edges.at(edge).get_node_to().get_id()) != installable_set.end();
            });
            auto b_installable = std::any_of(b_edges.begin(), b_edges.end(), [this](auto &edge) {
                return installable_set.find(graph.graph.edges.at(edge).get_node_to().get_id()) != installable_set.end();
            });
            return a_installable < b_installable;
        });

        //.map(|(version_set_id, edges)| {
        //                (
        //                    DisplayOp::Requirement(version_set_id, edges),
        //                    indenter.push_level(),
        //                )
        //            })

        std::vector<std::pair<DisplayOpVariant, Indenter>> stack;
        std::transform(chunked_vector.begin(), chunked_vector.end(), std::back_inserter(stack), [&indenter](auto &pair) {
            // pair.first is version_set_id
            // pair.second is vector of edges
            return std::make_pair(DisplayOp::Requirement{pair.first, pair.second}, indenter.push_level());
        });

        if (!stack.empty()) {
            // Mark the first element of the stack as not having any remaining siblings
            stack[0].second.set_last();
        }

        while (!stack.empty()) {
            auto node_indenter_pair = stack.back();
            stack.pop_back();
            auto top_level = node_indenter_pair.second.is_at_top_level();
            auto indent = node_indenter_pair.second.get_indent();

            std::visit([this, &oss, &reported, &stack, &node_indenter_pair, &top_level, &indent](auto &&arg) {
                using T = std::decay_t<decltype(arg)>;
                if constexpr (std::is_same_v<T, DisplayOp::Requirement>) {
                    auto requirement = std::any_cast<DisplayOp::Requirement>(arg);
                    auto version_set_id = requirement.version_set_id;
                    auto edges = requirement.edges;
                    assert(!edges.empty());

                    //let installable = edges.iter().any(|&e| {
                    //                        let (_, target) = graph.edge_endpoints(e).unwrap();
                    //                        installable_nodes.contains(&target)
                    //                    });

                    auto installable = std::any_of(edges.begin(), edges.end(), [this](auto &edge) {
                        auto edge_object = graph.graph.edges.at(edge);
                        auto target = edge_object.get_node_to();
                        return installable_set.find(target.get_id()) != installable_set.end();
                    });

                    auto req = pool->resolve_version_set(version_set_id);
                    auto name_id = pool->resolve_version_set_package_name(version_set_id);
                    auto name = pool->resolve_package_name(name_id);

                    auto target = graph.graph.edges.at(edges[0]).get_node_to();
                    auto missing = edges.size() == 1 && std::holds_alternative<ProblemNode::UnresolvedDependency>(target.get_payload());

                    if (missing) {
                        // No candidates for requirement
                        if (top_level) {
                            oss << indent << "No candidates were found for " << name << " " << req << "." << std::endl;
                        } else {
                            oss << indent << name << " " << req << ", for which no candidates were found." << std::endl;
                        }
                    } else if (installable) {
                        // Package can be installed (only mentioned for top-level requirements)
                        if (top_level) {
                            oss << indent << name << " " << req << " can be installed with any of the following options:" << std::endl;
                        } else {
                            oss << indent << name << " " << req << ", which can be installed with any of the following options:" << std::endl;
                        }

                        std::vector<EdgeIndex> filtered_edges;
                        std::copy_if(edges.begin(), edges.end(), std::back_inserter(filtered_edges), [this](auto &edge) {
                            return installable_set.find(graph.graph.edges.at(edge).get_node_to().get_id()) != installable_set.end();
                        });
                        std::vector<std::pair<DisplayOpVariant, Indenter>> children;
                        std::transform(filtered_edges.begin(), filtered_edges.end(), std::back_inserter(children), [this,&node_indenter_pair](auto &edge) {
                            return std::make_pair(DisplayOp::Candidate{graph.graph.edges.at(edge).get_node_to().get_id()}, node_indenter_pair.second.push_level());
                        });

                        std::vector<std::pair<DisplayOpVariant, Indenter>> deduplicated_children;
                        std::unordered_set<SolvableId> merged_and_seen;
                        for (auto &child : children) {
                            if (!std::holds_alternative<DisplayOp::Candidate>(child.first)) {
                                throw std::runtime_error("Unexpected child type");
                            }
                            auto child_node = std::get<DisplayOp::Candidate>(child.first);
                            auto child_node_index = child_node.node_index;
                            auto payload = graph.graph.get_node(child_node_index).get_payload();
                            auto solvable_id = std::get<ProblemNode::Solvable>(payload).solvable;
                            auto merged = merged_candidates.find(solvable_id);

                            if (merged_and_seen.find(solvable_id) != merged_and_seen.end()) {
                                continue;
                            }

                            if (merged != merged_candidates.end()) {
                                for (auto &id : merged->second.ids) {
                                    merged_and_seen.insert(id);
                                }
                            }

                            deduplicated_children.push_back(child);
                        }

                        if (!deduplicated_children.empty()) {
                            // Mark the first element of the stack as not having any remaining siblings
                            deduplicated_children[0].second.set_last();
                        }

                        stack.insert(stack.end(), deduplicated_children.begin(), deduplicated_children.end());

                    } else {
                        // Package cannot be installed (the conflicting requirement is further down the tree)
                        if (top_level) {
                            oss << indent << name << " " << req << " cannot be installed because there are no viable options:" << std::endl;
                        } else {
                            oss << indent << name << " " << req
                                << ", which cannot be installed because there are no viable options:" << std::endl;
                        }

                        //            let children: Vec<_> = edges
                        //                            .iter()
                        //                            .map(|&e| {
                        //                                (
                        //                                    DisplayOp::Candidate(graph.edge_endpoints(e).unwrap().1),
                        //                                    indenter.push_level(),
                        //                                )
                        //                            })
                        //                            .collect();

                        std::vector<std::pair<DisplayOpVariant, Indenter>> children;
                        std::transform(edges.begin(), edges.end(), std::back_inserter(children), [this,&node_indenter_pair](auto &edge) {
                            return std::make_pair(DisplayOp::Candidate{graph.graph.edges.at(edge).get_node_to().get_id()}, node_indenter_pair.second.push_level());
                        });

                        std::vector<std::pair<DisplayOpVariant, Indenter>> deduplicated_children;
                        std::unordered_set<SolvableId> merged_and_seen;

                        for (auto &child : children) {
                            if (!std::holds_alternative<DisplayOp::Candidate>(child.first)) {
                                throw std::runtime_error("Unexpected child type");
                            }
                            auto child_node = std::get<DisplayOp::Candidate>(child.first);
                            auto child_node_index = child_node.node_index;

                            auto payload = graph.graph.get_node(child_node_index).get_payload();
                            auto solvable_id = std::get<ProblemNode::Solvable>(payload).solvable;
                            auto merged = merged_candidates.find(solvable_id);

                            if (merged_and_seen.find(solvable_id) != merged_and_seen.end()) {
                                continue;
                            }

                            if (merged != merged_candidates.end()) {
                                for (auto &id: merged->second.ids) {
                                    merged_and_seen.insert(id);
                                }
                            }

                            deduplicated_children.push_back(child);
                        }

                        if (!deduplicated_children.empty()) {
                            // Mark the first element of the stack as not having any remaining siblings
                            deduplicated_children[0].second.set_last();
                        }

                        stack.insert(stack.end(), deduplicated_children.begin(), deduplicated_children.end());
                    }
                } else if constexpr (std::is_same_v<T, DisplayOp::Candidate>) {
                    auto candidate = std::any_cast<DisplayOp::Candidate>(arg);
                    auto node_index = candidate.node_index;
                    auto payload = graph.graph.get_node(node_index).get_payload();
                    auto solvable_id = std::get<ProblemNode::Solvable>(payload).solvable;

                    if (reported.find(solvable_id) != reported.end()) {
                        return;  // continue
                    }

                    auto solvable = pool->resolve_solvable(solvable_id);
                    auto name = pool->resolve_package_name(solvable.get_name_id());

                    //let version = if let Some(merged) = self.merged_candidates.get(&solvable_id) {
                    //                        reported.extend(merged.ids.iter().cloned());
                    //                        self.merged_solvable_display
                    //                            .display_candidates(&self.pool, &merged.ids)
                    //                    } else {
                    //                        self.merged_solvable_display
                    //                            .display_candidates(&self.pool, &[solvable_id])
                    //                    };

                    auto merged = merged_candidates.find(solvable_id);

                    std::string version;
                    if (merged != merged_candidates.end()) {
                        for (auto &id : merged->second.ids) {
                            reported.insert(id);
                        }
                        auto display_merged_solvable = DisplayMergedSolvable<VS, N>(pool, merged->second.ids);
                        version = display_merged_solvable.to_string();
                    } else {
                        auto display_candidates = DisplayMergedSolvable<VS, N>(pool, {solvable_id});
                        version = display_candidates.to_string();
                    }

                    std::optional<StringId> excluded;
                    for (auto &edge : graph.graph.outgoing_edges(node_index)) {
                        if (std::holds_alternative<ProblemEdge::Conflict>(edge.get_weight())) {
                            auto conflict = std::get<ProblemEdge::Conflict>(edge.get_weight());
                            if (std::holds_alternative<ConflictCause::Excluded>(conflict.conflict_cause)) {
                                auto target = edge.get_node_to().get_id();
                                auto target_payload = graph.graph.get_node(target).get_payload();
                                auto reason = std::get<ProblemNode::Excluded>(target_payload).excluded;
                                excluded = reason;
                            } else {
                                excluded = std::nullopt;
                            }
                        }
                    }

                    //let already_installed = graph.edges(candidate).any(|e| {
                    //                        e.weight() == &ProblemEdge::Conflict(ConflictCause::ForbidMultipleInstances)
                    //                    });

                    auto cand_edges = graph.graph.outgoing_edges(node_index);
                    auto already_installed = std::any_of(cand_edges.begin(), cand_edges.end(), [](auto &&edge) {
                        return std::visit([](auto &&arg) -> bool {
                            using T_arg = std::decay_t<decltype(arg)>;
                            if constexpr (std::is_same_v<T_arg, ProblemEdge::Conflict>) {
                                auto conflict_arg = std::any_cast<ProblemEdge::Conflict>(arg);
                                return std::holds_alternative<ConflictCause::ForbidMultipleInstances>(conflict_arg.conflict_cause);
                            } else {
                                return false;
                            }
                        }, edge.get_weight());
                    });


                    //let constrains_conflict = graph.edges(candidate).any(|e| {
                    //                        matches!(
                    //                            e.weight(),
                    //                            ProblemEdge::Conflict(ConflictCause::Constrains(_))
                    //                        )
                    //                    });

                    auto constrains_conflict = std::any_of(cand_edges.begin(), cand_edges.end(), [](auto &edge) {
                        if (std::holds_alternative<ProblemEdge::Conflict>(edge.get_weight())) {
                            auto conflict = std::get<ProblemEdge::Conflict>(edge.get_weight());
                            return std::holds_alternative<ConflictCause::Constrains>(conflict.conflict_cause);
                        }
                        return false;
                    });

                    // let is_leaf = graph.edges(candidate).next().is_none();
                    auto is_leaf = graph.graph.outgoing_edges(node_index).empty();

                    if (excluded.has_value()) {
                        auto excluded_reason = excluded.value();
                        oss << indent << name << " " << version << " is excluded because " << pool->resolve_string(excluded_reason) << std::endl;
                    } else if (is_leaf) {
                        oss << indent << name << " " << version << std::endl;
                    } else if (already_installed) {
                        oss << indent << name << " " << version << ", which conflicts with the versions reported above." << std::endl;
                    } else if (constrains_conflict) {

                        //let mut version_sets = graph
                        //                            .edges(candidate)
                        //                            .flat_map(|e| match e.weight() {
                        //                                ProblemEdge::Conflict(ConflictCause::Constrains(
                        //                                    version_set_id,
                        //                                )) => Some(version_set_id),
                        //                                _ => None,
                        //                            })
                        //                            .dedup()
                        //                            .peekable();

                        auto candidate_edges = graph.graph.outgoing_edges(node_index);
                        std::unordered_set<VersionSetId> version_sets;
                        std::transform(candidate_edges.begin(), candidate_edges.end(), std::inserter(version_sets, version_sets.end()), [](auto &edge) {
                            return std::visit([](auto &&arg) -> VersionSetId {
                                using T_arg = std::decay_t<decltype(arg)>;
                                if constexpr (std::is_same_v<T_arg, ProblemEdge::Conflict>) {
                                    auto conflict_arg = std::any_cast<ProblemEdge::Conflict>(arg);
                                    return std::get<ConflictCause::Constrains>(conflict_arg.conflict_cause).version;
                                } else {
                                    throw std::runtime_error("Unexpected edge type");
                                }
                            }, edge.get_weight());
                        });

                        oss << indent << name << " " << version << " would constrain" << std::endl;
                        auto temp_indenter = node_indenter_pair.second.push_level();
                        for (auto it = version_sets.begin(); it != version_sets.end(); ++it) {
                            auto version_set_id = *it;
                            auto version_set = pool->resolve_version_set(version_set_id);
                            auto version_set_package_name_id = pool->resolve_version_set_package_name(version_set_id);
                            auto version_set_package_name = pool->resolve_package_name(version_set_package_name_id);

                            if (std::next(it) == version_sets.end()) {
                                temp_indenter.set_last();
                            }

                            auto temp_indent = temp_indenter.get_indent();
                            oss << temp_indent << name << " " << version_set << ", which conflicts with any installable versions previously reported" << std::endl;
                        }
                    } else {
                        oss << indent << name << " " << version << " would require" << std::endl;

                        //let mut requirements = graph
                        //                            .edges(candidate)
                        //                            .chunk_by(|e| e.weight().requires())
                        //                            .into_iter()
                        //                            .map(|(version_set_id, group)| {
                        //                                let edges: Vec<_> = group.map(|e| e.id()).collect();
                        //                                (version_set_id, edges)
                        //                            })

                        std::vector<Edge<ProblemNodeVariant, ProblemEdgeVariant>> requires_edges;
                        auto node_outgoing_edges = graph.graph.outgoing_edges(node_index);
                        std::copy_if(node_outgoing_edges.begin(), node_outgoing_edges.end(), std::back_inserter(requires_edges), [](auto &edge) {
                            return std::holds_alternative<ProblemEdge::Requires>(edge.get_weight());
                        });

                        std::unordered_map<VersionSetId, std::vector<EdgeIndex>> chunked;
                        for (auto &edge : requires_edges) {
                            auto requires = std::get<ProblemEdge::Requires>(edge.get_weight());
                            if (chunked.find(requires.version_set_id) == chunked.end()) {
                                chunked[requires.version_set_id] = std::vector<EdgeIndex>();
                            }
                            chunked.at(requires.version_set_id).push_back(edge.get_id());
                        }

                        //                            .sorted_by_key(|(_version_set_id, edges)| {
                        //                                edges.iter().any(|&edge| {
                        //                                    installable_nodes
                        //                                        .contains(&graph.edge_endpoints(edge).unwrap().1)
                        //                                })
                        //                            })

                        std::vector<std::pair<VersionSetId, std::vector<EdgeIndex>>> chunked_vector(chunked.begin(), chunked.end());
                        std::sort(chunked_vector.begin(), chunked_vector.end(), [this](auto &a, auto &b) -> bool {
                            auto a_edges = a.second;
                            auto b_edges = b.second;
                            auto a_installable = std::any_of(a_edges.begin(), a_edges.end(), [this](auto &edge) {
                                return installable_set.find(graph.graph.edges.at(edge).get_node_to().get_id()) != installable_set.end();
                            });
                            auto b_installable = std::any_of(b_edges.begin(), b_edges.end(), [this](auto &edge) {
                                return installable_set.find(graph.graph.edges.at(edge).get_node_to().get_id()) != installable_set.end();
                            });
                            return a_installable < b_installable;
                        });

                        //                            .map(|(version_set_id, edges)| {
                        //                                (
                        //                                    DisplayOp::Requirement(version_set_id, edges),
                        //                                    indenter.push_level(),
                        //                                )
                        //                            })
                        //                            .collect::<Vec<_>>();


                        std::vector<std::pair<DisplayOpVariant, Indenter>> requirements;
                        std::transform(chunked_vector.begin(), chunked_vector.end(), std::back_inserter(requirements), [&node_indenter_pair](auto &pair) {
//                             pair.first is version_set_id
//                             pair.second is vector of edges
                            return std::make_pair(DisplayOp::Requirement{pair.first, pair.second}, node_indenter_pair.second.push_level());
                        });

                        if (!requirements.empty()) {
                            requirements[0].second.set_last();
                        }

                        stack.insert(stack.end(), requirements.begin(), requirements.end());
                    }
                }
            }, node_indenter_pair.first);

        }

        return oss.str();
    }

};

#endif // DISPLAY_UNSAT_H