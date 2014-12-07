-module(nodes).
-export([setup/0, start/0, coordinator/0, cohort/0]).

-record(node_state, {value}).


coordinator() -> coordinator([], #node_state{value=nil}).
coordinator(Cohorts, State) -> receive
        {add_cohort, Pid} -> coordinator(lists:append(Cohorts, [Pid]), State);
        {start_2pc_with_commit} -> query_to_commit(Cohorts)
    end.

cohort() -> cohort([], #node_state{value=nil}).
cohort(Cohorts, State) -> receive
        {propose_value, Value} -> cohort(Cohorts, #node_state{value=Value});
        {query, Coordinator} -> if
            State#node_state.value == commit -> Coordinator ! {agreement, yes}
        end,
        cohort(Cohorts, State)
    end.

query_to_commit(OtherNodes) ->
    lists:map(fun(Node) -> Node ! {query, self()} end, OtherNodes).

setup() ->
    A = spawn(nodes, coordinator, []),
    B = spawn(nodes, cohort, []),
    C = spawn(nodes, cohort, []),
    A ! {add_cohort, B},
    A ! {add_cohort, C},
    B ! {propose_value, commit}, 
    C ! {propose_value, commit}, 
    A ! {start_2pc_with_commit},
    nothing.
start() -> nothing.
