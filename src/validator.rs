#[derive(PartialEq, Debug)]
pub struct TODO;

// TODO: for much later: if you have a lot of vectors, etc. use custom allocators
#[derive(PartialEq, Debug)]
enum State {
    // how to encode what conditions are set outright, what are achievable and what are mutually exclusive?
    Start {
        name: StateName,
        set: Vec<Condition>,
        unset: Vec<Condition>,
        maybe: Vec<Condition>,
        maybe_set: Vec<Condition>,
        maybe_unset: Vec<Condition>,
        transitions: Vec<(StateIndexer, Vec<Condition>)>,
    },
    Base {
        name: StateName,
        requirements: Vec<Condition>,
        set: Vec<Condition>,
        unset: Vec<Condition>,
        maybe: Vec<Condition>,
        maybe_set: Vec<Condition>,
        maybe_unset: Vec<Condition>,
        transitions: Vec<(StateIndexer, Vec<Condition>)>,
    },
    Termination {
        name: StateName,
        requirements: Vec<Condition>,
    },
}

#[derive(PartialEq, Debug)]
pub struct States {
    states: Vec<State>,
}

// is it even necessary or is it only just glorified usize?
#[derive(PartialEq, Debug, Clone, Copy)]
struct StateIndexer(usize);

#[derive(Debug, PartialEq, Clone)]
struct IdentsIndexer(usize);
type Condition = IdentsIndexer;
type StateName = IdentsIndexer;

use std::ops::Index;
impl Index<StateIndexer> for States {
    type Output = State;
    fn index(&self, idx:StateIndexer) -> &Self::Output {
        &self.states[idx.0]
    }
}

// only check if there are transition paths leading to a state
// migth be useful to check some things step by step
pub fn check_unachievable_states(states: &States) -> Result<TODO, Vec<usize>> {
    let goal = states.states.len();
    let mut count = 0;
    let mut achieved = vec![false; goal];
    let mut checked = vec![false; goal];
    let mut unachievable = vec![];

    // used to check if no new paths have been discovered
    let mut prev_count = 0;
    'l: loop {
        for (i, state) in states.states.iter().enumerate() {
            match state {
                State::Start{transitions, ..} => if !checked[i] {
                    checked[i] = true; achieved[i] = true; count += 1;
                    for (idx, _) in transitions {
                        if !achieved[idx.0] {
                            achieved[idx.0] = true;
                            count += 1;
                            if count == goal {break 'l}
                        }
                    }
                },
                State::Base{transitions, ..} => if !checked[i] {
                    checked[i] = true;
                    for (idx, _) in transitions {
                        if !achieved[i] {unachievable.push(i); continue}
                        if !achieved[idx.0] {
                            achieved[idx.0] = true;
                            count += 1;
                            if count == goal {break 'l}
                        }
                    }
                },
                State::Termination{..} => if !achieved[i] {unachievable.push(i)}
            }
        }
        if count == prev_count {break}
        prev_count = count;
    }

    if unachievable.len() > 0 {
        let mut updated = vec![];
        for idx in unachievable {if !achieved[idx] {updated.push(idx)}}
        if updated.len() > 0 {return Err(updated)}
    }
    if count < goal {panic!("Not all states are achievable, yet we didn't find the unachievable ones")}
    Ok(TODO)
}

pub fn tmp_name_validate_conditions(states: &States, idents: &Idents) -> Result<TODO, TODO> {
    let n_conditions = idents.idents.len();

    let starting_indexes = {
        let mut starting_indexes = vec![];
        for i in 0..states.states.len() {
            if let State::Start{..} = &states.states[i] {
                starting_indexes.push(i);
            }
        }
        starting_indexes
    };
    assert!(starting_indexes.len() != 0);

    let mut checked_list: Vec<Combination> = vec![];

    's_states: for i in starting_indexes {
        let mut idx = StateIndexer(i);
        let mut path: Vec<Combination> = vec![];
        let mut conditions = BitArray::new(n_conditions);
        let mut maybe_set = BitArray::new(n_conditions);
        let mut maybe_unset = BitArray::new(n_conditions);

        let mut walk_back = false;
        let mut already_checked = false;
        let mut should_walk_back = false;
        let mut transition_idx = 0;

        'check: loop {
            // println!("\n--------------- next iteration ---------------");
            should_walk_back = false;
            if already_checked {
                // for p in &path {println!("{p:?}")}
                todo!("next iteration after already checking");
            }
            if walk_back {
                // should we pop or take the last?
                // println!("path before popping:");
                // for p in &path {println!("{p:?}")}
                // println!("path after popping:");
                match path.pop() {
                    Some(combination) => {
                        idx = combination.state;
                        conditions = combination.conditions;
                        maybe_set = combination.maybe_set;
                        maybe_unset = combination.maybe_unset;
                        transition_idx = combination.transition_idx + 1;
                    },
                    None => continue 's_states
                }
            }

            // for p in &path {println!("{p:?}")}
            // println!("current state idx: {:?}", idx.0);
            // println!("transition_idx: {transition_idx}");

            let (s_set, s_unset, s_maybe, s_maybe_set, s_maybe_unset, s_transitions) = {
                match &states[idx] {
                    State::Base{set, unset, maybe, maybe_set, maybe_unset, transitions, ..} |
                        State::Start{set, unset, maybe, maybe_set, maybe_unset, transitions, ..} => {
                            (set, unset, maybe, maybe_set, maybe_unset, transitions)
                        }
                    State::Termination{..} => {
                        // todo!("got to termination state, now go back")
                        // println!("TERMINATION");
                        walk_back = true;
                        continue 'check;
                    }
                }
            };

            // Step 1. and 2.
            if !walk_back {
                transition_idx = 0;
                // println!("zeroed transition_idx");

                // upadte conditions with what is set and possible changes with
                // what can and cannot change
                for cond in s_set {
                    conditions.set_at(cond.0);
                    maybe_set.unset_at(cond.0);
                    maybe_unset.unset_at(cond.0);
                }

                for cond in s_unset {
                    conditions.unset_at(cond.0);
                    maybe_set.unset_at(cond.0);
                    maybe_unset.unset_at(cond.0);
                }

                for cond in s_maybe {
                    if conditions.at(cond.0) {
                        maybe_unset.set_at(cond.0);
                    } else {
                        maybe_set.set_at(cond.0);
                    }
                }

                for cond in s_maybe_set {
                    if !conditions.at(cond.0) {
                        maybe_set.set_at(cond.0);
                    }
                }

                for cond in s_maybe_unset {
                    if conditions.at(cond.0) {
                        maybe_unset.set_at(cond.0);
                    }
                }
                // TODO: also check requirements for that state

                for c in &checked_list {
                    if idx == c.state && conditions == c.conditions &&
                        maybe_set == c.maybe_set && maybe_unset == c.maybe_unset
                    {
                        // println!("already checked");
                        // already_checked = true;
                        walk_back = true;
                        continue 'check;
                    }
                }

                // future TODO: handle the negative conditions / logical expressions
                // TODO: make those two into one loop
                let mut dead_end = true;
                't: for (_, conds) in s_transitions {
                    for cond in conds {
                        if !conditions.at(cond.0) || maybe_unset.at(cond.0) {continue 't;}
                    }
                    dead_end = false;
                }
                if dead_end {
                    todo!("report that the state can be a dead end and go back");
                    // TODO: report what conditions need to be set for what transitions
                }

                let mut unachievable_transitions = vec![];
                for (i, (_, conds)) in s_transitions.iter().enumerate() {
                    for cond in conds {
                        if !conditions.at(cond.0) && !maybe_set.at(cond.0) {
                            unachievable_transitions.push(i);
                        }
                    }
                }
                if unachievable_transitions.len() > 0 {
                    // TODO:
                    // don't error, because there might be a loop that that makes a transition
                    // possible, so instead note it in a variable and if at the end it is still
                    // unachievable, only then report error
                    /*
                    match &states[idx] {
                        State::Start{name, ..} | State::Base{name, ..} | State::Termination{name, ..} => {
                            // eprint!("in state: {} ", idents.name_at(name.0));
                            if unachievable_transitions.len() == 1 {
                                // eprint!("there is an unachievable transition ");
                            } else {
                                // eprintln!("there are unachievable transitions to:");
                            }
                        }
                    }
                    for t in unachievable_transitions {
                        match &states[s_transitions[t].0] {
                            State::Start{name, ..} | State::Base{name, ..} | State::Termination{name, ..} => {
                                // eprintln!("- {} (with index: {t})", idents.name_at(name.0));
                            }
                        }
                    }
                    // eprintln!("TODO: actually push it to some unachievable transitions for each state and at the end report what is the path to that state");
                    // */
                }
            }

            if (transition_idx < s_transitions.len()) && !should_walk_back {
                let combination = Combination {
                    state: idx,
                    transition_idx,
                    conditions: conditions.clone(),
                    maybe_set: maybe_set.clone(),
                    maybe_unset: maybe_unset.clone(),
                };
                path.push(combination.clone());
                checked_list.push(combination);
                idx = s_transitions[transition_idx].0;
                walk_back = false;

                // after pushing set the necessary conditions for exploring after transitioning
                for cond in &s_transitions[transition_idx].1 {
                    if !conditions.at(cond.0) && !maybe_set.at(cond.0) {
                        // println!("doesn't mean conditions for transition");
                        walk_back = true;
                        continue;
                    }
                    if !conditions.at(cond.0) {
                        assert!(maybe_set.at(cond.0), "if you got here you probably should be able to set those conditions");
                        // conditions from maybe_set -> set
                        conditions.set_at(cond.0);
                        maybe_set.unset_at(cond.0);
                        // if it has to be set, disallow considering it unset
                        maybe_unset.unset_at(cond.0);
                    }
                }
                continue 'check;
            } else {
                // println!("will walk back");
                walk_back = true;
            }
        }
    }

    unimplemented!();
}

#[derive(Debug, Clone)]
struct Combination {
    state: StateIndexer,
    transition_idx: usize,
    // default conditions to get to this point
    conditions: BitArray,
    // all the conditions that could be different at this point
    maybe_set: BitArray,
    maybe_unset: BitArray,
}

impl Combination {
    fn eq_for_checking_visited(&self, other: &Combination) -> bool {
        self.state == other.state && self.conditions == other.conditions
        && self.maybe_set == other.maybe_set && self.maybe_unset == other.maybe_unset
    }
}

// TODO: dump states into a graphviz file also can do that with "fully filled conditions" -
// - transition conditions take into consideration state requirements and if condition has to be
// met for all outgoing transition it can be added as a requirement to the state
// TODO: show all the dead ends, unachievable transitions, etc. on a graph

#[derive(Debug, Clone, PartialEq)]
struct BitArray {
    len: usize,
    bits: Vec<usize>
}

impl BitArray {
    fn new(len: usize) -> BitArray {
        let i: usize = len / usize::BITS as usize +
            if len % usize::BITS as usize == 0 { 0 }
            else { 1 };
        BitArray { len, bits: vec![0; i] }
    }

    fn at(&self, i: usize) -> bool {
        assert!(i < self.len, "index out of bounds");
        let idx = i / usize::BITS as usize;
        self.bits[idx] >> (i % usize::BITS as usize) & 0b1 != 0
    }

    fn set_at(&mut self, i: usize) {
        assert!(i < self.len, "index out of bounds");
        let idx = i / usize::BITS as usize;
        self.bits[idx] |= 1 << (i % usize::BITS as usize);
    }

    fn unset_at(&mut self, i: usize) {
        assert!(i < self.len, "index out of bounds");
        let idx = i / usize::BITS as usize;
        self.bits[idx] &= !(1 << (i % usize::BITS as usize));
    }

    fn flip(&mut self, i: usize) -> bool {
        assert!(i < self.len, "index out of bounds");
        let idx = i / usize::BITS as usize;
        self.bits[idx] ^= 1 << (i % usize::BITS as usize);
        self.bits[idx] >> (i % usize::BITS as usize) & 0b1 != 0
    }

    fn and(&mut self, other: &BitArray) {
        assert!(self.len <= other.len, "len of other must be at least as much as len of self");
        for i in 0..self.bits.len() {
            self.bits[i] &= other.bits[i]
        }
    }

    fn or(&mut self, other: &BitArray) {
        assert!(self.len <= other.len, "len of other must be at least as much as len of self");
        for i in 0..self.bits.len() {
            self.bits[i] |= other.bits[i]
        }
    }

    fn is_zero(&self) -> bool {
        for i in 0..self.bits.len() {
            if self.bits[i] > 0 {return false}
        }
        true
    }
    
    fn zero_out(&mut self) {
        for i in 0..self.bits.len() {
            self.bits[i] = 0;
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IdentData {
    idx: usize,
    len: usize,
}

#[derive(PartialEq)]
struct Ident<'a> {
    slice: IdentData,
    parent: &'a Idents
}

#[derive(Debug, PartialEq)]
pub struct Idents {
    chars: Vec<u8>,
    pub idents: Vec<IdentData>,
}

impl<'s> Idents {
    fn new() -> Idents {
        Idents {
            chars: vec![],
            idents: vec![],
        }
    }

    fn at(&'s self, i: usize) -> Ident<'s> {
        Ident {
            slice: self.idents[i].clone(),
            parent: &self
        }
    }

    // push if not already in and return index of ident
    fn add_if_not_in(&mut self, chars: &[u8]) -> usize {
        'i: for i in 0..self.idents.len() {
            let ident = &self.idents[i];
            if ident.len != chars.len() {continue 'i}
            for c in 0..chars.len() {
                if chars[c] != self.chars[ident.idx + c] {continue 'i}
            }
            return i;
        }

        self.idents.push(IdentData{idx: self.chars.len(), len: chars.len()});
        for &c in chars {
            self.chars.push(c);
        }
        return self.idents.len()-1;
    }

    pub fn name_at(&self, i: usize) -> &str {
        let idx = self.idents[i].idx;
        let len = self.idents[i].len;
        match std::str::from_utf8(&self.chars[idx..idx+len]) {
            Ok(s) => s,
            Err(_) => panic!("invalid ident {i}, at index: {idx} with length: {len}"),
        }
    }
}

impl<'s> Ident<'s> {
    fn chars(&self) -> &'s [u8] {
        &self.parent.chars[self.slice.idx .. self.slice.idx + self.slice.len]
    }
}

pub mod parser {
    use crate::validator::*;

    // trait for checking if alphanumeric or '_' because rust doesn't allow methods on primitives
    trait IdentChar { fn is_ident(&self) -> bool; }
    impl IdentChar for u8 {
        fn is_ident(&self) -> bool {
            self.is_ascii_alphanumeric() || *self == b'_'
        }
    }

    pub enum InputType {
        File,
        String,
    }

    // tmp empty, should return why there was an error
    // TODO: maybe a vector of error enums, maybe with positions or something like that
    // then the caller can decide to print error messages
    pub fn parse_config(input: &str, input_type: InputType) -> Result<(States, Idents), TODO> {
        let contents = match input_type {
            InputType::File => match std::fs::read_to_string(input) {
                Ok(c) => c,
                Err(e) => {eprintln!("{e}"); return Err(TODO);}
            }
            InputType::String => input.to_string()
        };

        let (tokens, idents) = match tokenize(&contents) {
            Ok((tk, id)) => (tk, id),
            Err(_) => todo!("error reporting")
        };

        match parse_states(&tokens) {
            Ok(s) => Ok((s, idents)),
            Err(_) => todo!("handle error")
        }
    }

    #[derive(Debug)]
    struct Token {
        row: usize,
        col: usize,
        t: TokenType,
    }

    #[derive(Debug)]
    enum TokenType {
        Ident(usize),
        Start,
        Termination,
        Set,
        Unset,
        MaySet,
        MayUnset,
        MayChange,
        ParenO,
        ParenC,
        CurlyO,
        CurlyC,
        BracketO,
        BracketC,
        Comma,
        Colon,
        AngleC,
    }

    fn tokenize(input: &str) -> Result<(Vec<Token>, Idents), TODO> {
        let chars = input.as_bytes();
        let mut tokens = vec![];
        let mut idents = Idents::new();
        let mut row = 1;
        let mut rb = 0;

        let mut i = 0;
        while i < chars.len() {
            match chars[i] {
                char if char.is_ascii_whitespace() => (),
                b'(' => tokens.push( Token {row, col: i-rb+1, t: TokenType::ParenO}),
                b')' => tokens.push( Token {row, col: i-rb+1, t: TokenType::ParenC}),
                b'{' => tokens.push( Token {row, col: i-rb+1, t: TokenType::CurlyO}),
                b'}' => tokens.push( Token {row, col: i-rb+1, t: TokenType::CurlyC}),
                b'[' => tokens.push( Token {row, col: i-rb+1, t: TokenType::BracketO}),
                b']' => tokens.push( Token {row, col: i-rb+1, t: TokenType::BracketC}),
                b',' => tokens.push( Token {row, col: i-rb+1, t: TokenType::Comma}),
                b':' => tokens.push( Token {row, col: i-rb+1, t: TokenType::Colon}),
                b'>' => tokens.push( Token {row, col: i-rb+1, t: TokenType::AngleC}),
                b'+' => tokens.push( Token {row, col: i-rb+1, t: TokenType::Set}),
                b'-' => tokens.push( Token {row, col: i-rb+1, t: TokenType::Unset}),
                b'?' => {
                    if i+1 < chars.len() {
                        match chars[i+1] {
                            b'+' => {tokens.push( Token {row, col: i-rb+1, t: TokenType::MaySet}); i += 1}
                            b'-' => {tokens.push( Token {row, col: i-rb+1, t: TokenType::MayUnset}); i += 1}
                            _ => tokens.push( Token {row, col: i-rb+1, t: TokenType::MayChange}),
                        }
                    } else {todo!("error reporting")}
                }
                b'\n' => (), // ignore, because if something returns at '\n', it will advance `i` and won't catch it
                b'/' => match chars[i+1] {
                    b'/' => while i < chars.len() {
                        i += 1; if chars[i] == b'\n' {break}
                    },
                    _ => panic!("invalid comment (subject to change)")
                },

                char if char.is_ident() => {
                    let mut len = 0;
                    while i+len < chars.len() && chars[i+len].is_ident() {len+=1}
                    match &chars[i..i+len] {
                        b"Start" => tokens.push(Token{row, col: i-rb+1, t: TokenType::Start}),
                        b"Termination" => tokens.push(Token{row, col: i-rb+1, t: TokenType::Termination}),
                        _ => {
                            let ident_idx = idents.add_if_not_in(&chars[i..i+len]);
                            tokens.push(Token {row, col: i-rb+1, t: TokenType::Ident(ident_idx)});
                        }
                    }
                    i += len-1;
                },
                _ => todo!("error reporting")
            }
            if chars[i] == b'\n' {row += 1; rb = i+1;}
            i += 1;
        }

        Ok((tokens, idents))
    }

    fn parse_states(tokens: &[Token]) -> Result<States, TODO> {
        let (mut start_states, mut base_states, mut termination_states) = (vec![], vec![], vec![]);
        let mut i = 0;
        while i < tokens.len() {
            match tokens[i].t {
                TokenType::Start => { i += 1;
                    match parse_list::<StateInt>(&tokens[i..]) {
                        Ok((offset, s_states)) => {
                            i += offset;
                            start_states = s_states;
                        }
                        Err(_) => todo!("error reporting")
                    }
                }
                TokenType::Termination => { i += 1;
                    match parse_list::<StateInt>(&tokens[i..]) {
                        Ok((offset, t_states)) => {
                            i += offset;
                            termination_states = t_states;
                        }
                        Err(_) => todo!("error reporting")
                    }
                }
                TokenType::Ident(_) => {
                    match parse_state(&tokens[i..]) {
                        Ok((offset, state)) => {
                            i += offset;
                            base_states.push(state);
                        }
                        Err(_) => todo!("error reporting")
                    }
                }
                _ => {
                    eprintln!("expected state, `Start` block or `Termination` block");
                    todo!("error reporting")
                }
            }
            i += 1;
        }

        // update transitions to reflect indeces in states, not indeces in identifiers
        let mut states = vec![];
        for state in &start_states {
            assert!(state.requirements.len() == 0);
            let mut transitions = vec![];
            't: for tr in &state.transitions {
                let mut offset = start_states.len();
                for (i, st) in base_states.iter().enumerate() {
                    if tr.name == st.name {transitions.push((StateIndexer(i+offset), tr.conditions.clone())); continue 't}
                }
                offset += base_states.len();
                for (i, st) in termination_states.iter().enumerate() {
                    if tr.name == st.name {transitions.push((StateIndexer(i+offset), tr.conditions.clone())); continue 't}
                }
                todo!("error reporting")
            }
            states.push(State::Start {
                name: state.name.clone(),
                set: state.set.clone(),
                unset: state.unset.clone(),
                maybe: state.maybe.clone(),
                maybe_set: state.maybe_set.clone(),
                maybe_unset: state.maybe_unset.clone(),
                transitions
            })
        }

        for state in &base_states {
            let mut requirements = vec![];
            for req in &state.requirements {
                requirements.push(req.clone());
            }

            let mut transitions = vec![];
            't: for tr in &state.transitions {
                let mut offset = start_states.len();
                for (i, st) in base_states.iter().enumerate() {
                    if tr.name == st.name {transitions.push((StateIndexer(i+offset), tr.conditions.clone())); continue 't}
                }
                offset += base_states.len();
                for (i, st) in termination_states.iter().enumerate() {
                    if tr.name == st.name {transitions.push((StateIndexer(i+offset), tr.conditions.clone())); continue 't}
                }
                todo!("error reporting")
            }
            states.push(State::Base {
                name: state.name.clone(),
                requirements,
                set: state.set.clone(),
                unset: state.unset.clone(),
                maybe: state.maybe.clone(),
                maybe_set: state.maybe_set.clone(),
                maybe_unset: state.maybe_unset.clone(),
                transitions
            })
        }

        for state in &termination_states {
            assert!(state.transitions.len() == 0);
            let mut requirements = vec![];
            for req in &state.requirements {
                requirements.push(req.clone());
            }
            states.push(State::Termination {
                name: state.name.clone(),
                requirements
            })
        }

        Ok(States{states})
    }

    #[derive(Debug)]
    struct StateInt {
        name: StateName,
        requirements: Vec<ReqName>,
        set: Vec<Condition>,
        unset: Vec<Condition>,
        maybe: Vec<Condition>,
        maybe_set: Vec<Condition>,
        maybe_unset: Vec<Condition>,
        transitions: Vec<Transition>,
    }

    #[derive(Debug)]
    struct Transition {
        name: StateName,
        conditions: Vec<Condition>
    }
    // might not be necessary
    type ReqName = IdentsIndexer;

    trait ListParsable: Sized + 'static + std::fmt::Debug {
        fn parse_item(tokens: &[Token]) -> Result<(usize, Self), TODO>;
    }

    impl ListParsable for StateInt {
        fn parse_item(tokens: &[Token]) -> Result<(usize, StateInt), TODO> {
            parse_state(tokens)
        }
    }

    impl ListParsable for Transition {
        fn parse_item(tokens: &[Token]) -> Result<(usize, Transition), TODO> {
            if tokens.len() < 2 {todo!("error reporting")}
            if let TokenType::Ident(idx) = tokens[0].t {
                if let TokenType::Colon = tokens[1].t {
                    let i = 2;
                    match parse_list::<Condition>(&tokens[i..]) {
                        Ok((offset, conditions)) => {
                            return Ok((i+offset, Transition{name: IdentsIndexer(idx), conditions}))
                        },
                        Err(TODO) => todo!("error reporting")
                    }
                } else {
                    Ok((0, Transition {name: IdentsIndexer(idx), conditions: vec![]}))
                }
            } else {Err(TODO)}
        }
    }

    impl ListParsable for IdentsIndexer {
        fn parse_item(tokens: &[Token]) -> Result<(usize, Self), TODO> {
            if tokens.len() < 1 {todo!("error reporting")}
            if let TokenType::Ident(idx) = tokens[0].t {
                return Ok((0, Self (idx)))
            } else {todo!("error reporting")}
        }
    }

    fn parse_list<T: ListParsable>(tokens: &[Token]) -> Result<(usize, Vec<T>), TODO> {
        use std::any::TypeId;
        let mut multiple = false;
        let mut list: Vec<T> = vec![];

        let mut i = 0;
        while i < tokens.len() {
            match tokens[i].t {
                TokenType::BracketC | TokenType::ParenC => return Ok((i, list)),
                TokenType::BracketO => {multiple = true}
                TokenType::ParenO => if TypeId::of::<T>() == TypeId::of::<ReqName>() {
                    multiple = true;
                } else {todo!("error reporting")}
                TokenType::Comma => if !multiple {todo!("error reporting")}
                TokenType::Colon => todo!(),
                TokenType::Ident(_) => match T::parse_item(&tokens[i..]) {
                    Ok((offset, name)) => {
                        i += offset;
                        list.push(name);
                        if !multiple {return Ok((i, list))}
                    }
                    Err(_) => todo!()
                }

                TokenType::CurlyO | TokenType::CurlyC | TokenType::AngleC => todo!(),
                TokenType::Set | TokenType::Unset | TokenType::MaySet | TokenType::MayUnset | TokenType::MayChange => todo!(),
                TokenType::Start | TokenType::Termination => panic!("list should never start with `Start` or `Termination` block")
            }
            i += 1;
        }
        unimplemented!("what to do when you run out of tokens?");
    }

    fn parse_state(tokens: &[Token]) -> Result<(usize, StateInt), TODO> {
        let mut name = None;
        let mut block_opened = false;
        let mut requirements = vec![];
        let mut set = vec![];
        let mut unset = vec![];
        let mut maybe = vec![];
        let mut maybe_set = vec![];
        let mut maybe_unset = vec![];
        let mut transitions = vec![];

        let mut i = 0;
        while i < tokens.len() {
            match tokens[i].t {
                TokenType::Ident(idx) => if name == None {
                    name = Some(idx)
                } else {todo!("parse state internals")}
                TokenType::ParenO => match parse_list::<ReqName>(&tokens[i..]) {
                    Ok((offset, mut conditions)) => {
                        i += offset;
                        requirements.append(&mut conditions);
                    }
                    Err(_) => todo!()
                }
                TokenType::ParenC => todo!(),
                TokenType::CurlyO => {block_opened = true}
                TokenType::CurlyC => {
                    if !block_opened {todo!("error reporting")}
                    if let Some(idx) = name {
                        return Ok((i, StateInt{
                            name: IdentsIndexer(idx),
                            requirements, set, unset, maybe, maybe_set, maybe_unset, transitions
                        }))
                    } else {todo!("error reporting")}
                },

                TokenType::BracketO => todo!(),
                TokenType::BracketC => todo!(),
                TokenType::Comma => todo!(),
                TokenType::Colon => todo!(),

                TokenType::AngleC => {
                    if i+1 >= tokens.len() {eprintln!("expected list but no tokens found"); return Err(TODO)}
                    i += 1;
                    match parse_list::<Transition>(&tokens[i..]) {
                        Ok((offset, mut trs)) => {
                            i += offset;
                            transitions.append(&mut trs);
                        }
                        _ => todo!()
                    }
                }

                TokenType::Set | TokenType::Unset |
                TokenType::MaySet | TokenType::MayUnset | TokenType::MayChange => {
                    if i+1 >= tokens.len() {eprintln!("expected list but no tokens found"); return Err(TODO)}
                    let tp = &tokens[i].t;
                    i += 1;
                    match parse_list::<Condition>(&tokens[i..]) {
                        Ok((offset, mut list)) => {
                            i += offset;
                            match tp {
                                TokenType::Set => set.append(&mut list),
                                TokenType::Unset => unset.append(&mut list),
                                TokenType::MaySet => maybe_set.append(&mut list),
                                TokenType::MayUnset => maybe_unset.append(&mut list),
                                TokenType::MayChange => maybe.append(&mut list),
                                _ => unreachable!("it should never be another type")
                            }
                        }
                        _ => todo!()
                    }
                }

                TokenType::Start => {
                    eprintln!("state name cannot use the keyword `Start`");
                    return Err(TODO)
                }
                TokenType::Termination => {
                    eprintln!("state name cannot use the keyword `Termination`");
                    return Err(TODO)
                }
            }
            i += 1;
        }
        unimplemented!();
    }
}

// TODO: add something (;) to denote that state has no interior - useful for termination
