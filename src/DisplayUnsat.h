#ifndef DISPLAY_UNSAT_H
#define DISPLAY_UNSAT_H

#include <sstream>
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
    explicit DisplayUnsat(std::shared_ptr<Pool<VS, N>> poolRef, const ProblemGraph &graphRef,
                           const std::unordered_map<SolvableId, MergedProblemNode> &merged_candidatesRef,
                           const std::unordered_set<NodeIndex> &installable_setRef,
                           const std::unordered_set<NodeIndex> &missing_setRef)
            : pool(poolRef), graph(graphRef) {
        merged_candidates = graph.simplify(pool);
        installable_set = graph.get_installable_set();
        missing_set = graph.get_missing_set();
    }

    friend std::ostream &operator<<(std::ostream &os, const DisplayUnsat &display_unsat) {
        os << display_unsat.to_string();
        return os;
    }

    std::string fmt_graph(std::unordered_set<Edge<ProblemNodeVariant, ProblemEdgeVariant>> top_level_edges, bool top_level_indent) const {
        std::ostringstream oss;

        auto reported = std::unordered_set<SolvableId>();
        // Note: we are only interested in requires edges here
        auto indenter = Indenter(top_level_indent);

        auto requires_edges = std::copy_if(top_level_edges, [](auto &edge) {
            return std::holds_alternative<ProblemEdge::Requires>(edge.get_weight());
        });

        std::unordered_map<VersionSetId, std::vector<Edge<ProblemNodeVariant, ProblemEdgeVariant>>> chunked;
        for (auto &edge : requires_edges) {
            auto requires = std::get<ProblemEdge::Requires>(edge.get_weight());
            chunked[requires.version_set_id].push_back(edge);
        }

        std::sort(chunked.begin(), chunked.end(), [this](auto &a, auto &b) {
            auto a_edges = a.second;
            auto b_edges = b.second;
            auto a_installable = std::any_of(a_edges.begin(), a_edges.end(), [this](auto &edge) {
                return installable_set.find(edge.get_node_to().get_id()) != installable_set.end();
            });
            auto b_installable = std::any_of(b_edges.begin(), b_edges.end(), [this](auto &edge) {
                return installable_set.find(edge.get_node_to().get_id()) != installable_set.end();
            });
            return a_installable > b_installable;
        });

        std::vector<std::pair<DisplayOpVariant, ChildOrder>> stack = std::map(chunked.begin(), chunked.end(), [&indenter](auto &pair) {
            // pair.first is version_set_id
            // pair.second is vector of edges
            return std::make_pair(DisplayOp::Requirement{pair.first, pair.second}, indenter.push_level());
        });

        if (!stack.empty()) {
            // Mark the first element of the stack as not having any remaining siblings
            stack[0].second = ChildOrder::Last;
        }


        // TODO: Implement this
        return oss.str();
    }

};

#endif // DISPLAY_UNSAT_H