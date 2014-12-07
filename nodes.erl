-module(nodes).
-export([setup/0, start/0, one_node/0]).

-record(node_state, {value}).

one_node() -> one_node([], #node_state{value=nil}).
one_node(Cohorts, State) -> receive
        {add_cohort, Pid} -> one_node(lists:append(Cohorts, [Pid]), State);
        {propose_value, Value} -> one_node(Cohorts, #node_state{value=Value})
    end.

setup() ->
    A = spawn(nodes, one_node, []),
    B = spawn(nodes, one_node, []),
    C = spawn(nodes, one_node, []),
    A ! {add_cohort, B},
    A ! {add_cohort, C},
    A ! {propose_value, commit}, 
    nothing.
start() -> nothing.
