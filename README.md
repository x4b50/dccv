# Dependency Chain Completeness Validator

It is a W.I.P. tool for validating state machine-like dependency chains.
Each state system consists of starting and terminating states, internal states, transitions between them and conditions, which govern what transitions and state can be achieved.
Each state can have an internal state system that can set higher order conditions and the termination of an internal system is itself a condition that can be checked.

I'm at this point unsure if it is worth making the distinction between conditions and already achieved states; probably the only difference between them will be in the ease of use and conceptual separation.
Once difference that can be semantically meaningful is that state must be active to influence what transitions can be taken, while a condition can be set at earlier states and checked/changed later.

## Implementation todos

- [x] parse and validate the most baisc, conditionless system
- [ ] add string literal names to allow spaces
- [ ] do proper error handling and error reporting
- [ ] logical operations on conditions

- [x] Possible starting and terminating states

Each state (except above) must declare:
- [x] what conditions must be met to enable transitioning to it (as of 24.6.2025 only parsed, not checked)
- [x] what conditions must be met to advance to another state
- [x] what conditions can be/are fulfilled while in it
- [ ] what other states can occur at the same time (must be mutually declared) (should there be something like that?)
- [ ] what states are exclusive to each other (might be achieved by having one state be a negative condition of transitioning to another, but explicit mutual exclusivity is preferable)
- [ ] if any of the conditions or outgoing states are mutually exclusive (should only be needed if mutually active states are a default, but I'm not sure at this point)
-  ...
- [ ] state can have another state system inside, where:
    - [ ] internal termination states are also conditions, which can be checked on a higher level
    - [ ] within internal states it is possible to interact with higher level conditions 

Incompatibility of conditions must be declared.
Should the scope of all conditions and states be global, or should there be name spacing for recursive systems?

Checker must validate that:
- [x] each state is achievable 
- [x] each state transition is achievable (as of 24.6.2025 there isn't really any reporting of that but it is being checked)
- [ ] no two incompatible states/conditions can be achieved at the same time / in sequence 
States and conditions might have an only one way temporal incompatibility 
(There might be a distinction between states that are incompatible in tandem and in general)

Output:
- [ ] violations in set rules 
- [ ] places where ambiguity occurs
- ...
- [ ] when all conditions are satisfied, can as an option output a graph/version of input with all transition conditions explicitly defined
