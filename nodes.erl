-module(nodes).
-export([setup/0, start/0, coordinator/0, cohort/0]).

-record(coordinator_state, {decisions_basket}).
-record(cohort_state, {decision}).


coordinator() -> coordinator([], #coordinator_state{decisions_basket=[]}).
coordinator(Cohorts, State) -> receive
        {add_cohort, Pid} -> coordinator(lists:append(Cohorts, [Pid]), State);
        {start_2pc_with_commit} -> query_to_commit(Cohorts), coordinator(Cohorts, State);
        {agreement, yes} ->
            Basket = lists:append(
               State#coordinator_state.decisions_basket,
               [yes]
            ),
            coordinator(Cohorts, State#coordinator_state{decisions_basket=Basket})
    end.

cohort() -> cohort([], #cohort_state{decision=nil}).
cohort(Cohorts, State) -> receive
        {propose_decision, Decision} -> cohort(Cohorts, #cohort_state{decision=Decision});
        {query, Coordinator} -> if
            State#cohort_state.decision == commit -> Coordinator ! {agreement, yes}
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
    B ! {propose_decision, commit}, 
    C ! {propose_decision, commit}, 
    A ! {start_2pc_with_commit},
    nothing.
start() -> nothing.
