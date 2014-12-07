-module(nodes).
-export([setup/0, start/0, one_node/0]).

-record(node_state, {value}).

one_node() -> one_node([], #node_state{value=nil}).
one_node(Cohorts, State) -> receive
        {add_cohort, Pid} -> one_node(lists:append(Cohorts, [Pid]), State);
        {propose_value, Value} -> one_node(Cohorts, #node_state{value=Value});
        {start_2pc} -> query_to_commit(Cohorts);
        {query, Coordinator} -> if
            State#node_state.value == commit -> Coordinator ! {agreement, yes}
        end,
        one_node(Cohorts, State)
    end.

query_to_commit(OtherNodes) ->
    lists:map(fun(Node) -> Node ! {query, self()} end, OtherNodes).

setup() ->
    A = spawn(nodes, one_node, []),
    B = spawn(nodes, one_node, []),
    C = spawn(nodes, one_node, []),
    A ! {add_cohort, B},
    A ! {add_cohort, C},
    B ! {propose_value, commit}, 
    C ! {propose_value, commit}, 
    A ! {start_2pc},
    nothing.
start() -> nothing.
