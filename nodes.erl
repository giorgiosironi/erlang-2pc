-module(nodes).
-export([test_commit/0, test_abort/0, coordinator/0, cohort/0]).

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
        {agreement, Agreement} ->
            log("As coordinator received a yes"),
            Basket = lists:append(
               State#coordinator_state.decisions_basket,
               [Agreement]
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
            broadcast(Cohorts, {commit, self()}),
            wait_acknowledgements(Cohorts, commit);
        true ->
            broadcast(Cohorts, {abort, self()}),
            wait_acknowledgements(Cohorts, abort)
    end.

wait_acknowledgements(Cohorts, FinalState) ->
    log_final_state(FinalState).

log_final_state(FinalState) ->
    if 
        FinalState == commit -> log("COMMIT!");
        FinalState == abort -> log("ABORT!")
    end.

cohort() -> cohort([], #cohort_state{decision=nil}).
cohort(Cohorts, State) -> receive
        {propose_decision, Decision} ->
            log("Will propose: ~p", [Decision]),
            cohort(Cohorts, #cohort_state{decision=Decision});
        {query, Coordinator} ->
            log("Queried by coordinator"),
            Coordinator ! {agreement, State#cohort_state.decision},
            cohort(Cohorts, State);
        {commit, Coordinator} ->
            log_final_state(commit),
            Coordinator ! {acknowledgement};
        {abort, Coordinator} ->
            log_final_state(abort),
            Coordinator ! {acknowledgement}
    end.

query_to_commit(OtherNodes) ->
    broadcast(OtherNodes, {query, self()}).

broadcast(Nodes, Message) ->
    lists:map(fun(Node) -> Node ! Message end, Nodes).

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

test_commit() -> 
    A = spawn(nodes, coordinator, []),
    B = spawn(nodes, cohort, []),
    C = spawn(nodes, cohort, []),
    A ! {add_cohort, B},
    A ! {add_cohort, C},

    B ! {propose_decision, yes}, 
    C ! {propose_decision, yes},
    A ! {start_2pc_with_commit}.

test_abort() -> 
    A = spawn(nodes, coordinator, []),
    B = spawn(nodes, cohort, []),
    C = spawn(nodes, cohort, []),
    A ! {add_cohort, B},
    A ! {add_cohort, C},

    B ! {propose_decision, yes}, 
    C ! {propose_decision, no},
    A ! {start_2pc_with_commit}.
