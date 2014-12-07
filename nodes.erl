-module(nodes).
-export([setup/0, start/0, one_node/1]).

one_node(Cohorts) -> receive
        {add_cohort, Pid} -> one_node(lists:append(Cohorts, [Pid]))
    end.

setup() ->
    A = spawn(nodes, one_node, [[]]),
    B = spawn(nodes, one_node, [[]]),
    C = spawn(nodes, one_node, [[]]),
    A ! {add_cohort, B},
    A ! {add_cohort, C},
    nothing.
start() -> nothing.
