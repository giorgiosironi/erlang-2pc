-module(nodes).
-export([start/0, coordinator/0, cohort/0]).

-record(coordinator_state, {decisions_basket}).
-record(cohort_state, {decision}).


coordinator() -> coordinator([], #coordinator_state{decisions_basket=[]}).
coordinator(Cohorts, State) -> receive
        {add_cohort, Pid} -> 
            log("As coordinator added cohort: ~p", [Pid]),
            coordinator(lists:append(Cohorts, [Pid]), State);
        {start_2pc_with_commit} ->
            log("As coordinator, 1st phase trying to commit"),
            query_to_commit(Cohorts), coordinator(Cohorts, State);
        {agreement, yes} ->
            log("As coordinator received a yes"),
            Basket = lists:append(
               State#coordinator_state.decisions_basket,
               [yes]
            ),
            VotingFinished = length(Basket) == length(Cohorts),
            if 
                VotingFinished -> completion(Cohorts, Basket);
                true -> coordinator(Cohorts, State#coordinator_state{decisions_basket=Basket})
            end
    end.

completion(Cohorts, Basket) -> 
    log("As coordinator, 2nd phase"),
    Commit = lists:all(fun(Agreement) -> Agreement == yes end, Basket),
    if
        % TODO: broadcast(Cohorts, Message)
        Commit ->
            lists:map(fun(Node) -> Node ! {commit} end, Cohorts),
            log("COMMIT!")
    end.

cohort() -> cohort([], #cohort_state{decision=nil}).
cohort(Cohorts, State) -> receive
        {propose_decision, Decision} ->
            log("Will propose: ~p", [Decision]),
            cohort(Cohorts, #cohort_state{decision=Decision});
        {query, Coordinator} ->
            log("Queried by coordinator"),
            if
                State#cohort_state.decision == commit -> Coordinator ! {agreement, yes}
            end,
        cohort(Cohorts, State);
        {commit} -> log("COMMIT!")
    end.

query_to_commit(OtherNodes) ->
    lists:map(fun(Node) -> Node ! {query, self()} end, OtherNodes).

log(String) -> log(String, []).
log(String, Arguments) ->
    io:fwrite(
      string:join(
        [
            "~p - ",
            String,
            "~n"
        ],
        ""
      ),
      lists:append(
        [self()],
        Arguments
      )
    ).

start() -> 
    A = spawn(nodes, coordinator, []),
    B = spawn(nodes, cohort, []),
    C = spawn(nodes, cohort, []),
    A ! {add_cohort, B},
    A ! {add_cohort, C},

    B ! {propose_decision, commit}, 
    C ! {propose_decision, commit},
    A ! {start_2pc_with_commit}.
