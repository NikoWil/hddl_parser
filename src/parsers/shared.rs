use chumsky::prelude::*;

use super::ast::{
    AtomicFormulaSkeleton, BaseType, Predicate, PrimitiveType, SubtaskId, TaskSymbol, Term, Type,
    TypedList, Variable,
};

pub fn parse_name() -> impl Parser<char, String, Error = Simple<char>> {
    text::ident::<_, Simple<char>>().padded()
}

// <base-type> ::= <name>
pub fn parse_basetype() -> impl Parser<char, BaseType, Error = Simple<char>> {
    parse_name().map(|name| BaseType { name })
}

// <atomic-formula-skeleton> ::=
//     (<predicate> <typed list (variable)>)
pub fn parse_atomicformulaskeleton(
) -> impl Parser<char, AtomicFormulaSkeleton, Error = Simple<char>> {
    parse_predicate()
        .padded()
        .then(parse_typedlist(parse_variable()))
        .delimited_by(just('(').padded(), just(')').padded())
        .map(|(predicate, typed_list)| AtomicFormulaSkeleton {
            predicate,
            typed_list,
        })
}

// <predicate> ::= <name>
pub fn parse_predicate() -> impl Parser<char, Predicate, Error = Simple<char>> {
    parse_name().map(|name| Predicate { name })
}

// <variable> ::= ?<name>
pub fn parse_variable() -> impl Parser<char, Variable, Error = Simple<char>> {
    just('?')
        .ignore_then(parse_name())
        .padded()
        .map(|name| Variable { name })
}

// <typed list (x)> ::= x+ - <type>
//     [<typed list (x)>]
pub fn parse_typedlist<T>(
    parser: impl Parser<char, T, Error = Simple<char>>,
) -> impl Parser<char, TypedList<T>, Error = Simple<char>> {
    parser
        .padded()
        .repeated()
        .at_least(1)
        .then(parse_type())
        .repeated()
        .at_least(1)
        .map(|elems| TypedList { elems })
}

// <primitive-type> ::= <name>
pub fn parse_primitivetype() -> impl Parser<char, PrimitiveType, Error = Simple<char>> {
    parse_name().map(|name| PrimitiveType { name })
}

// <type> ::= (either <primitive-type>+)
// <type> ::= <primitive-type>
pub fn parse_type() -> impl Parser<char, Type, Error = Simple<char>> {
    parse_primitivetype()
        .padded()
        .repeated()
        .at_least(1)
        .delimited_by(
            just('(').padded().then(just("either").padded()),
            just(')').padded(),
        )
        .map(|types| Type::Either(types))
        .or(parse_primitivetype().map(|ptype| Type::Simple(ptype)))
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
