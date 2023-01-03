use chumsky::prelude::*;

use super::ast::{BaseType, Predicate, PrimitiveType, SubtaskId, TaskSymbol, Term, Variable};

pub fn parse_name() -> impl Parser<char, String, Error = Simple<char>> {
    text::ident::<_, Simple<char>>().padded()
}

// <base-type> ::= <name>
pub fn parse_basetype() -> impl Parser<char, BaseType, Error = Simple<char>> {
    parse_name().map(|name| BaseType { name })
}

// <predicate> ::= <name>
pub fn parse_predicate() -> impl Parser<char, Predicate, Error = Simple<char>> {
    parse_name().map(|name| Predicate { name })
}

// <variable> ::= ?<name>
pub fn parse_variable() -> impl Parser<char, Variable, Error = Simple<char>> {
    just('?')
        .ignore_then(parse_name())
        .map(|name| Variable { name })
}

// <primitive-type> ::= <name>
pub fn parse_primitivetype() -> impl Parser<char, PrimitiveType, Error = Simple<char>> {
    parse_name().map(|name| PrimitiveType { name })
}

// <task-symbol> ::= <name>
pub fn parse_tasksymbol() -> impl Parser<char, TaskSymbol, Error = Simple<char>> {
    parse_name().map(|name| TaskSymbol { name })
}

// <subtask-id> ::= <name>
pub fn parse_subtaskid() -> impl Parser<char, SubtaskId, Error = Simple<char>> {
    parse_name().map(|name| SubtaskId { name })
}

// <term> ::= <name>
pub fn parse_term() -> impl Parser<char, Term, Error = Simple<char>> {
    parse_name().map(|name| Term { name })
}
