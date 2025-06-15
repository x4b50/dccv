#[allow(dead_code)]

// TODO: for much later: if you have a lot of vectors, etc. use custom allocators
#[derive(PartialEq, Debug)]
struct State {
    // requirements: Vec<Condition>,
    // how to encode what conditions are set outright, what are achievable and what are mutually exclusive?
    // conditions: Vec<Condition>,
    transitions: Vec<(StateIndexer, Vec<Condition>)>,
    // TODO: something with what other states are exclusive to this one
}

// TODO: change to static &str
// TODO: use a hashmap for name -> idx
#[derive(PartialEq, Debug)]
pub struct States {
    states: Vec<State>,
}

// should conditions be strings w/ just their names, hashes or something else?
#[derive(PartialEq, Debug)]
struct Condition;
#[derive(PartialEq, Debug)]
struct StateIndexer { v: usize }
// TODO: backpatching indexes while parsing states will be an issue

impl States {
    fn new() -> States {
        States {states: vec![]}
    }

    fn push(&mut self, state: State) {
        self.states.push(state);
    }
}

use std::ops::Index;
impl Index<StateIndexer> for States {
    type Output = State;
    fn index(&self, idx:StateIndexer) -> &Self::Output {
        &self.states[idx.v]
    }
}

use std::collections::HashMap;
#[derive(Debug, PartialEq)]
pub struct Table {
    // TODO: change to str
    indexes: HashMap<String, usize>,
    names: Vec<String>
}

// to ensure proper handling such that output of hashmap is an index to the vec
impl Table {
    fn new() -> Self {
        Table {
            indexes: HashMap::new(),
            names: vec![],
        }
    }

    fn try_insert(&mut self, v: String) -> Result<(), usize> {
        if self.indexes.contains_key(&v) {return Err(self.indexes[&v])}
        self.indexes.insert(v.clone(), self.names.len());
        self.names.push(v);
        Ok(())
    }

    fn has_name(&self, v: &str) -> bool {
        self.indexes.contains_key(v)
    }

    fn has_index(&self, i: usize) -> bool {
        i < self.names.len()
    }

    fn at(&self, i: usize) -> &str {
        if i >= self.names.len() {panic!("index out of bounds")}
        &self.names[i]
    }

    fn index_of(&self, v: &str) -> usize {
        if !self.has_name(v) {panic!("no value: '{v}' in Table")}
        self.indexes[v]
    }
}

// TODO: during parsing each state and condition identifier should be first gathered in a list of
// string, after that they should be assigned indeces 
// 
// ? array of strings, where index is index to state or hashmap


pub mod parser {
    use crate::validator::*;

    macro_rules! cvec {
        ($v:tt) => {
            $v.chars().collect::<Vec<char>>()
        }
    }

    // trait for checking if alphanumeric of '_' because rust doesn't allow methods on primitives
    trait Ident { fn is_ident(&self) -> bool; }
    impl Ident for char {
        fn is_ident(&self) -> bool {
            self.is_ascii_alphanumeric() || *self == '_'
        }
    }

    // tmp empty, should return why there was an error
    // TODO: vector of error enums, maybe with positions or something like that
    // then the caller can decide to print error messages
    pub fn parse_config(input_path: &str) -> Result<(States, Table), ()> {
        let contents = match std::fs::read_to_string(input_path) {
            Ok(c) => cvec!(c),
            Err(e) => {eprintln!("{e}"); return Err(());}
        };

        let mut state_names = Table::new();
        let (starting_states, states, termination_states);

        match parse_states(&contents) {
            Ok((_offset, (sts_s, sts, sts_t))) => {
                (starting_states, states, termination_states) = (sts_s, sts, sts_t);
                for state in starting_states.iter().chain(states.iter()).chain(termination_states.iter()) {
                    match state_names.try_insert(String::from_iter(state.name.iter())) {
                        Ok(()) => (),
                        Err(i) => {
                            eprintln!("redefiniton of state: {}", state_names.at(i));
                            return Err(())
                        }
                    }
                }
            }
            Err(_) => return Err(())
        }

        let mut states_patched = States::new();
        // backpatch the transition names in StateInt to indexes in State
        for state in starting_states.iter().chain(states.iter()) {
            let mut transitions = vec![];
            for tr in &state.transitions {
                let name = String::from_iter(tr.iter());
                if state_names.has_name(&name) {
                    transitions.push((
                            StateIndexer {v:state_names.index_of(&name)},
                            vec![]));
                } else {
                    eprintln!("undefined state: {}", name);
                    return Err(())
                }
            }
            states_patched.push(State {transitions});
        }

        Ok((states_patched, state_names))
    }

    fn advance_comment(contents: &[char]) -> usize {
        if contents.len() < 1 {return 0;}
        let mut i = 0;
        if contents[0] == '/' {
            while i < contents.len() {
                i+=1; if contents[i] == '\n' {break}
            }
        }
        return i+1;
    }

    fn parse_states(contents: &[char]) -> Result<(usize, (Vec<StateInt>, Vec<StateInt>, Vec<StateInt>)), ()> {
        let mut start_states = vec![];
        let mut states = vec![];
        let mut termination_states = vec![];

        let mut i = 0;
        while i < contents.len() {
            match contents[i] {
                char if char.is_whitespace() => (),
                '/' => {i += advance_comment(&contents[i+1..])}

                'S' if contents[i..].starts_with(&cvec!("Start")) => {
                    i += 5;
                    match parse_list(&contents[i..], ListElements::Starting) {
                        Ok((offset, list)) => {
                            i += offset;
                            start_states.append(&mut list.into_iter().map(|x| match x {
                                ListTypes::StateInt(s) => s,
                                ListTypes::Chars(_) =>
                                    panic!("parsing a list of states shouldn't return `Chars`")
                            }).collect());
                        }
                        Err(_) => return Err(())
                    }
                }

                'T' if contents[i..].starts_with(&cvec!("Termination")) => {
                    i += 11;
                    match parse_list(&contents[i..], ListElements::Terminating) {
                        Ok((offset, list)) => {
                            i += offset;
                            termination_states.append(&mut list.into_iter().map(|x| match x {
                                ListTypes::StateInt(s) => s,
                                ListTypes::Chars(_) =>
                                    panic!("parsing a list of states shouldn't return `Chars`")
                            }).collect());
                        }
                        Err(_) => return Err(())
                    }
                }

                char if char.is_ident() => {
                    match parse_state(&contents[i..], StateType::State) {
                        Ok((offset, state)) => {
                            i += offset;
                            states.push(state);
                        }
                        Err(_) => return Err(())
                    }
                }

                _ => todo!("failed with input {:?}", &contents[i..])
            }

            i += 1;
        }
        Ok((i, (start_states, states, termination_states)))
    }

    enum ListElements {
        Starting,
        StateName,
        Terminating,
    }

    #[derive(Debug)]
    enum ListTypes<'a> {
        Chars(&'a [char]),
        StateInt(StateInt<'a>),
    }

    // returns position of character AFTER the list
    fn parse_list(contents: &[char], of_what: ListElements)
        -> Result<(usize, Vec<ListTypes>), ()>
    {
        let mut multiple = false;
        let mut list: Vec<ListTypes> = vec![];

        let mut i = 0;
        while i < contents.len() {
            match contents[i] {
                char if char.is_whitespace() => (),
                '/' => {i += advance_comment(&contents[i+1..])}
                '[' => {multiple = true;},
                ']' => {
                    // allows for empty lists `[]`
                    return Ok((i, list))
                }
                ',' => {
                    // for now it technically doesn't do anything, because all list elements have a
                    // termination point of their own
                    if !multiple {
                        return Err(())
                    }
                }
                char if char.is_ident() => {
                    match of_what {
                        ListElements::StateName => {
                            let mut j = 0;
                            while i+j < contents.len() && contents[i+j].is_ident() {j+=1;}
                            list.push(ListTypes::Chars(&contents[i..i+j]));
                            i += j-1;
                            if !multiple {
                                return Ok((i, list))
                            }
                        }
                        ListElements::Starting => {
                            match parse_state(&contents[i..], StateType::Start) {
                                Ok((offset, state)) => {
                                    i += offset;
                                    list.push(ListTypes::StateInt(state));
                                },
                                Err(_) => return Err(())
                            }
                        }
                        ListElements::Terminating => {
                            match parse_state(&contents[i..], StateType::Termination) {
                                Ok((offset, state)) => {
                                    i += offset;
                                    list.push(ListTypes::StateInt(state));
                                },
                                Err(_) => return Err(())
                            }
                        }
                    }
                },
                _ => todo!("failed with input {:?}", &contents[i..])
            }
            i += 1;
        }
        return Ok((i, list));
    }

    #[derive(PartialEq, Copy, Clone)]
    enum StateType {
        Start,
        Termination,
        State,
    }

    fn parse_state(contents: &[char], t: StateType) -> Result<(usize, StateInt), ()> {
        let mut ident = None;
        let mut block = None;

        let mut i = 0;
        while i < contents.len() {
            match contents[i] {
                char if char.is_whitespace() => (),
                '/' => {i += advance_comment(&contents[i+1..])}

                char if char.is_ident() => {
                    if ident == None {
                        let mut j = 0;
                        while i+j < contents.len() && contents[i+j].is_ident() {j+=1;}
                        ident = Some(&contents[i..i+j]);
                        i += j-1;
                    } else {return Err(())}
                },

                '{' => { i += 1;
                    match parse_block(&contents[i..], t) {
                        Ok((offset, b)) => {
                            i += offset;
                            block = Some(b);
                        }
                        Err(_) => return Err(())
                    }
                }

                '}' => {
                    if let Some(block) = block {
                        if let Some(ident) = ident {
                            return Ok((i, StateInt {
                                name: ident,
                                transitions: block.transition_names
                            }))
                        } else {return Err(())}
                    } else {return Err(())}
                },
                _ => todo!("failed with input {:?}", &contents[i..])
            }
            i += 1;
        }
        unimplemented!("{ident:?}: {block:?}");
    }

    #[derive(Debug)]
    struct StateInt<'a> {
        name: &'a [char],
        transitions: Vec<&'a[char]>
    }

    #[derive(Debug)]
    struct Block<'a> {
        transition_names: Vec<&'a[char]>,
    }

    fn parse_block(contents: &[char], t: StateType) -> Result<(usize, Block), ()> {
        let mut i = 0;
        while i < contents.len() {
            match contents[i] {
                char if char.is_whitespace() => (),
                '/' => {i += advance_comment(&contents[i+1..])}
                '>' => { i += 1;
                    if t == StateType::Termination {return Err(())}
                    match parse_list(&contents[i..], ListElements::StateName) {
                        Ok((offset, list)) => {
                            if list.len() <= 0 {return Err(())}
                            let transition_names = list.into_iter().map(|x| match x {
                                ListTypes::Chars(c) => c,
                                ListTypes::StateInt(_) =>
                                    panic!("parsing list elements `StateName` shouldn't return any `StateInt`")
                            }).collect();
                            return Ok((i+offset, Block { transition_names }))
                        },
                        Err(_) => return Err(())
                    }
                }
                '}' => {
                    if t == StateType::Termination {
                        return Ok((0, Block { transition_names:vec![] }))
                    } else {return Err(())}
                }
                _ => todo!("failed with input {:?}", &contents[i..])
            }
            i += 1;
        }
        unimplemented!();
    }
}
