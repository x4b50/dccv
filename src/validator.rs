#[allow(dead_code)]

// TODO: for much later: if you have a lot of vectors, etc. use custom allocators
#[derive(PartialEq, Debug)]
enum State {
    // how to encode what conditions are set outright, what are achievable and what are mutually exclusive?
    Start {
        name: StateIdent,
        transitions: Vec<(StateIndexer, Vec<Condition>)>,
    },
    Base {
        name: StateIdent,
        requirements: Vec<Condition>,
        transitions: Vec<(StateIndexer, Vec<Condition>)>,
    },
    Termination {
        name: StateIdent,
        requirements: Vec<Condition>,
    },
}

#[derive(PartialEq, Debug)]
pub struct States {
    states: Vec<State>,
}

#[derive(PartialEq, Debug)]
struct Condition(usize);
#[derive(PartialEq, Debug)]
struct StateIndexer(usize);
#[derive(PartialEq, Debug)]
struct StateIdent(usize);

use std::ops::Index;
impl Index<StateIndexer> for States {
    type Output = State;
    fn index(&self, idx:StateIndexer) -> &Self::Output {
        &self.states[idx.0]
    }
}

pub fn check_unachievable_states(states: &States) -> Result<(), Vec<usize>> {
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
    Ok(())
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
    pub fn parse_config(input: &str, input_type: InputType) -> Result<(States, Idents), ()> {
        let contents = match input_type {
            InputType::File => match std::fs::read_to_string(input) {
                Ok(c) => c,
                Err(e) => {eprintln!("{e}"); return Err(());}
            }
            InputType::String => input.to_string()
        };

        let (tokens, idents) = match tokenize(&contents) {
            Ok((tk, id)) => (tk, id),
            Err(_) => return Err(())
        };

        match parse_states(&tokens, &idents) {
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
        ParenO,
        ParenC,
        CurlyO,
        CurlyC,
        BracketO,
        BracketC,
        Comma,
        AngleC,
    }

    #[derive(Clone, Debug, PartialEq)]
    struct IdentData {
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
        idents: Vec<IdentData>,
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

        fn name_at(&self, i: usize) -> &str {
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

    fn tokenize(input: &str) -> Result<(Vec<Token>, Idents), ()> {
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
                b'>' => tokens.push( Token {row, col: i-rb+1, t: TokenType::AngleC}),
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
                _ => return Err(())
            }
            if chars[i] == b'\n' {row += 1; rb = i+1;}
            i += 1;
        }

        Ok((tokens, idents))
    }

    fn parse_states(tokens: &[Token], idents: &Idents) -> Result<States, ()> {
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
                        Err(_) => return Err(())
                    }
                }
                TokenType::Termination => { i += 1;
                    match parse_list::<StateInt>(&tokens[i..]) {
                        Ok((offset, t_states)) => {
                            i += offset;
                            termination_states = t_states;
                        }
                        Err(_) => return Err(())
                    }
                }
                TokenType::Ident(id) => {
                    match parse_state(&tokens[i..]) {
                        Ok((offset, state)) => {
                            i += offset;
                            base_states.push(state);
                        }
                        Err(_) => return Err(())
                    }
                }
                _ => {
                    eprintln!("expected state, `Start` block or `Termination` block");
                    return Err(())
                }
            }
            i += 1;
        }

        let mut states = vec![];
        for state in &start_states {
            assert!(state.requirements.len() == 0);
            let mut transitions = vec![];
            't: for tr_name in &state.transitions {
                let mut offset = start_states.len();
                for (i, st) in base_states.iter().enumerate() {
                    if *tr_name == st.name {transitions.push((StateIndexer(i+offset), vec![])); continue 't}
                }
                offset += base_states.len();
                for (i, st) in termination_states.iter().enumerate() {
                    if *tr_name == st.name {transitions.push((StateIndexer(i+offset), vec![])); continue 't}
                }
                eprintln!("returned");
                return Err(())
            }
            states.push(State::Start {
                name: StateIdent(state.name.0),
                transitions
            })
        }

        for state in &base_states {
            let mut requirements = vec![];
            for req in &state.requirements {
                requirements.push(Condition(req.0));
            }

            let mut transitions = vec![];
            't: for tr_name in &state.transitions {
                let mut offset = start_states.len();
                for (i, st) in base_states.iter().enumerate() {
                    if *tr_name == st.name {transitions.push((StateIndexer(i+offset), vec![])); continue 't}
                }
                offset += base_states.len();
                for (i, st) in termination_states.iter().enumerate() {
                    if *tr_name == st.name {transitions.push((StateIndexer(i+offset), vec![])); continue 't}
                }
                eprintln!("returned");
                return Err(())
            }
            states.push(State::Base {
                name: StateIdent(state.name.0),
                requirements,
                transitions
            })
        }

        for state in &termination_states {
            assert!(state.transitions.len() == 0);
            let mut requirements = vec![];
            for req in &state.requirements {
                requirements.push(Condition(req.0));
            }
            states.push(State::Termination {
                name: StateIdent(state.name.0),
                requirements
            })
        }

        Ok(States{states})
    }

    #[derive(Debug)]
    struct StateInt {
        name: StateName,
        requirements: Vec<ReqName>,
        transitions: Vec<StateName>, // TODO add contidions
    }

    #[derive(Debug, PartialEq)]
    struct IdentsIndexer(usize);
    type ReqName = IdentsIndexer;
    type CondName = IdentsIndexer;
    type StateName = IdentsIndexer;

    trait ListParsable: Sized + 'static + std::fmt::Debug {
        fn parse_item(tokens: &[Token]) -> Result<(usize, Self), ()>;
    }

    impl ListParsable for StateInt {
        fn parse_item(tokens: &[Token]) -> Result<(usize, StateInt), ()> {
            parse_state(tokens)
        }
    }

    impl ListParsable for IdentsIndexer {
        fn parse_item(tokens: &[Token]) -> Result<(usize, Self), ()> {
            if tokens.len() < 1 {return Err(())}
            if let TokenType::Ident(idx) = tokens[0].t {
                return Ok((0, Self (idx)))
            } else {return Err(())}
        }
    }

    fn parse_list<T: ListParsable>(tokens: &[Token]) -> Result<(usize, Vec<T>), ()> {
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
                } else {return Err(())}
                TokenType::Comma => if !multiple {return Err(())}
                TokenType::Ident(_) => match T::parse_item(&tokens[i..]) {
                    Ok((offset, name)) => {
                        i += offset;
                        list.push(name);
                        if !multiple {return Ok((i, list))}
                    }
                    Err(_) => todo!()
                }
                TokenType::CurlyO | TokenType::CurlyC | TokenType::AngleC => todo!(),
                TokenType::Start | TokenType::Termination => panic!("list should never start with `Start` or `Termination` block")
            }
            i += 1;
        }
        unimplemented!("what to do when you run out of tokens?");
    }

    fn parse_state(tokens: &[Token]) -> Result<(usize, StateInt), ()> {
        let mut name = None;
        let mut block_opened = false;
        let mut requirements = vec![];
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
                    if !block_opened {return Err(())}
                    if let Some(idx) = name {
                        return Ok((i, StateInt{name: IdentsIndexer(idx), requirements, transitions}))
                    } else {return Err(())}
                },
                TokenType::BracketO => todo!(),
                TokenType::BracketC => todo!(),
                TokenType::Comma => todo!(),
                TokenType::AngleC => {
                    if i+1 >= tokens.len() {eprintln!("expected list but no tokens found"); return Err(())}
                    i += 1;
                    match parse_list::<StateName>(&tokens[i..]) {
                        Ok((offset, mut trs)) => {
                            i += offset;
                            transitions.append(&mut trs);
                        }
                        _ => todo!()
                    }
                }
                TokenType::Start => {
                    eprintln!("state name cannot use the keyword `Start`");
                    return Err(())
                }
                TokenType::Termination => {
                    eprintln!("state name cannot use the keyword `Termination`");
                    return Err(())
                }
            }
            i += 1;
        }
        unimplemented!();
    }
}
