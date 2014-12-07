-module(nodes).
-export([setup/0, start/0, one_node/2]).

one_node(Cohorts, State) -> receive
        {add_cohort, Pid} -> one_node(lists:append(Cohorts, [Pid]), State);
        {propose_value, Value} -> one_node(Cohorts, State)
    end.

setup() ->
    A = spawn(nodes, one_node, [[], nil]),
    B = spawn(nodes, one_node, [[], nil]),
    C = spawn(nodes, one_node, [[], nil]),
    A ! {add_cohort, B},
    A ! {add_cohort, C},
    nothing.
start() -> nothing.
