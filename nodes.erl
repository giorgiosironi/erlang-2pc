-module(nodes).

one_node() -> receive
        {add_neighbor, _Pid} -> node()
    end.
