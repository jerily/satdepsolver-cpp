#ifndef DISPLAY_CLAUSE_H
#define DISPLAY_CLAUSE_H

#include <sstream>
#include "Solvable.h"
#include "Pool.h"
#include "solver/Clause.h"
#include "DisplaySolvable.h"
#include "DisplayName.h"
#include "DisplayVersionSet.h"

// The DisplaySolvable class template is used to visualize a solvable
template<typename VS, typename N>
class DisplayClause {
private:
    std::shared_ptr<Pool<VS, N>> pool;
    ClauseState clause;

public:
// Constructor
    explicit DisplayClause(std::shared_ptr<Pool<VS, N>> poolRef, const ClauseState &clauseRef)
            : pool(poolRef), clause(clauseRef) {}

    friend std::ostream &operator<<(std::ostream &os, const DisplayClause &display_clause) {
        os << display_clause.to_string();
        return os;
    }

    std::string to_string() const {
        std::ostringstream oss;
        std::visit([this, &oss](auto &&arg) {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, Clause::InstallRoot>) {
                oss << "install root";
            } else if constexpr (std::is_same_v<T, Clause::Excluded>) {
                auto excluded = std::any_cast<Clause::Excluded>(arg);
                auto solvable_id = excluded.candidate;
                auto reason = excluded.reason;
                auto display_solvable = DisplaySolvable<VS, N>(pool, pool->resolve_internal_solvable(solvable_id));
                oss << display_solvable.to_string() << " excluded because " << pool->resolve_string(reason);
            } else if constexpr (std::is_same_v<T, Clause::Learnt>) {
                auto learnt = std::any_cast<Clause::Learnt>(arg);
                auto learnt_id = learnt.learnt_clause_id;
                oss << "learnt clause " << learnt_id.to_string() << " ";
            } else if constexpr (std::is_same_v<T, Clause::Requires>) {
                auto requires = std::any_cast<Clause::Requires>(arg);
                auto solvable_id = requires.parent;
                auto match_spec_id = requires.requirement;
                auto match_spec = pool->resolve_version_set(match_spec_id);
                auto display_parent = DisplaySolvable<VS, N>(pool, pool->resolve_internal_solvable(solvable_id));
                auto display_package_name = DisplayName<VS, N>(pool, pool->resolve_version_set_package_name(match_spec_id));
                oss << display_parent.to_string() << " requires " << display_package_name << " ";
                oss << match_spec;
            } else if constexpr (std::is_same_v<T, Clause::Constrains>) {
                auto constrains = std::any_cast<Clause::Constrains>(arg);
                auto s1 = constrains.parent;
                auto s2 = constrains.forbidden_solvable;
                auto vs_set_id = constrains.via;
                auto display_s1 = DisplaySolvable<VS, N>(pool, pool->resolve_internal_solvable(s1));
                auto display_s2 = DisplaySolvable<VS, N>(pool, pool->resolve_internal_solvable(s2));
                auto display_vs = DisplayVersionSet<VS, N>(pool, pool->resolve_version_set(vs_set_id));
                oss << display_s1.to_string() << " excludes " << display_s2.to_string() << " by " << display_vs.to_string();
            } else if constexpr (std::is_same_v<T, Clause::Lock>) {
                auto lock = std::any_cast<Clause::Lock>(arg);
                auto locked = lock.locked_candidate;
                auto forbidden = lock.other_candidate;
                auto display_locked = DisplaySolvable<VS, N>(pool, pool->resolve_internal_solvable(locked));
                auto display_forbidden = DisplaySolvable<VS, N>(pool, pool->resolve_internal_solvable(forbidden));
                oss << display_locked.to_string() << " is locked, so " << display_forbidden.to_string() << " is forbidden";
            } else if constexpr (std::is_same_v<T, Clause::ForbidMultipleInstances>) {
                auto forbid_multiple_instances = std::any_cast<Clause::ForbidMultipleInstances>(arg);
                auto s1 = forbid_multiple_instances.candidate;
                auto name = pool->resolve_internal_solvable(s1).get_solvable_unchecked().get_name_id();
                auto display_name = DisplayName<VS, N>(pool, name);
                oss << "only one " << display_name.to_string() << " is allowed";
            }
        }, clause.kind_);
        return oss.str();
    }

};

#endif // DISPLAY_CLAUSE_H