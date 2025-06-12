# Dependency Chain Completeness Validator


## Implementation todos

- [ ] Possible starting and terminating conditions

Each state (except above) must declare:
- [ ] what conditions must be met to achieve it
- [ ] what conditions can be/are fulfilled while in it (having achieved this state is also another potential condition)
- [ ] what conditions must be met to advance to another state and if that advancement is reversible / two-way 
- [ ] what other states can occur at the same time (must be mutually declared)
- [ ] what states are exclusive to each other (might be achieved by having one state be a negative condition of another, but explicit mutual exclusivity is preferable)
- [ ] if any of the conditions or out-coming states are mutually exclusive
-  ...
- [ ] state systems can be recursive within a given state:
    - [ ] internal termination states are also conditions
    - [ ] within internal states it is possible higher level conditions 

- incompatibility of conditions must be declared 

Checker must validate that:
- [ ] each state and state transition is achievable 
- [ ] no two incompatible states/conditions can be achieved at the same time / in sequence 
States and conditions might have an only one way temporal incompatibility 

(There might be a distinction between states that are incompatible in tandem and in general)

Output:
- [ ] violations in set rules 
- [ ] places where ambiguity occurs
- ...
- [ ] when all conditions are satisfied, can as an option output a graph/version of input with all transition conditions explicitly defined
