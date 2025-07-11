use dccv::prelude::*;

#[test]
fn test_all_states_achievable() {
    let states = parse_config("tests/basic_all_states_achievable.input", InputType::File);
    assert_ne!(states, Err(TODO), "pasring failed");

    let (states, _state_names) = states.unwrap();
    assert_eq!(check_unachievable_states(&states), Ok(TODO), "unachievable states found");
}

#[test]
fn test_states_unachievable() {
    let states = parse_config("tests/basic_states_unachievable.input", InputType::File);
    assert_ne!(states, Err(TODO), "pasring failed");

    let (states, _state_names) = states.unwrap();
    let unachievable = check_unachievable_states(&states);
    assert_ne!(unachievable, Ok(TODO), "unachievable states should have been found");
    assert_eq!(unachievable, Err(vec![2]), "wrong state found unachievable")
}

#[test]
fn test_unordered_declaration() {
    let states = parse_config("tests/basic_unordered_declaration.input", InputType::File);
    assert_ne!(states, Err(TODO), "pasring failed");

    let (states, _state_names) = states.unwrap();
    assert_eq!(check_unachievable_states(&states), Ok(TODO), "unachievable states found");
}
