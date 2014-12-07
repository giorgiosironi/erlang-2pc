Rough implementation of 2 Phase Commit between Erlang processes.

One process is the Coordinator, two other processes are Cohorts. All three have to agree whether to commit or to abort a decision.
http://en.wikipedia.org/wiki/Two-phase_commit_protocol

Limitations of this implementation:
* no timeouts
* no failure management (with durability of decisions taken by nodes with a write-ahead log)
