use chumsky::{prelude::*, text::whitespace};

use super::ast::{
    AtomicFormula, AtomicFormulaSkeleton, BaseType, ConstantsDef, ConstraintDef, Literal,
    OrderingDef, PEffect, Predicate, PredicatesDef, PrimitiveType, SubtaskDef, SubtaskId, TaskDef,
    TaskSymbol, Term, Type, TypedList, Types, TypesDef, Variable, GD,
};

pub fn parse_name() -> impl Parser<char, String, Error = Simple<char>> + Clone {
    text::ident::<_, Simple<char>>().padded()
}

// <types-def> ::= (:types <types>+)
pub fn parse_typesdef() -> impl Parser<char, TypesDef, Error = Simple<char>> + Clone {
    parse_types()
        .padded()
        .repeated()
        .at_least(1)
        .delimited_by(just('(').padded().then(just(":types")), just(')').padded())
        .map(|types| TypesDef { types })
}

// <types> ::= <typed list (name)>
//     | <base-type>
pub fn parse_types() -> impl Parser<char, Types, Error = Simple<char>> + Clone {
    parse_typedlist(parse_name())
        .map(|list| Types::List(list))
        .or(parse_basetype().map(|btype| Types::Base(btype)))
}

// <base-type> ::= <name>
pub fn parse_basetype() -> impl Parser<char, BaseType, Error = Simple<char>> + Clone {
    parse_name().map(|name| BaseType { name })
}

// <constants-def> ::=
//     (:constants <typed list (name)>)
pub fn parse_constantsdef() -> impl Parser<char, ConstantsDef, Error = Simple<char>> + Clone {
    parse_typedlist(parse_name())
        .delimited_by(
            just('(').then(just(":constants").padded()),
            just(')').padded(),
        )
        .map(|constants| ConstantsDef { constants })
}

// <predicates-def> ::=
//     (:predicates <atomic-formula-skeleton>+)
pub fn parse_predicatesdef() -> impl Parser<char, PredicatesDef, Error = Simple<char>> + Clone {
    parse_atomicformulaskeleton()
        .padded()
        .repeated()
        .at_least(1)
        .delimited_by(
            just('(').padded().then(just(":predicates")),
            just(')').padded(),
        )
        .map(|predicates| PredicatesDef { predicates })
}

// <atomic-formula-skeleton> ::=
//     (<predicate> <typed list (variable)>)
pub fn parse_atomicformulaskeleton(
) -> impl Parser<char, AtomicFormulaSkeleton, Error = Simple<char>> + Clone {
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
pub fn parse_predicate() -> impl Parser<char, Predicate, Error = Simple<char>> + Clone {
    parse_name().map(|name| Predicate { name })
}

// <variable> ::= ?<name>
pub fn parse_variable() -> impl Parser<char, Variable, Error = Simple<char>> + Clone {
    just('?')
        .ignore_then(parse_name())
        .padded()
        .map(|name| Variable { name })
}

// <typed list (x)> ::= x+ - <type>
//     [<typed list (x)>]
pub fn parse_typedlist<T>(
    parser: impl Parser<char, T, Error = Simple<char>> + Clone,
) -> impl Parser<char, TypedList<T>, Error = Simple<char>> + Clone {
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
pub fn parse_primitivetype() -> impl Parser<char, PrimitiveType, Error = Simple<char>> + Clone {
    parse_name().map(|name| PrimitiveType { name })
}

// <type> ::= (either <primitive-type>+)
// <type> ::= <primitive-type>
pub fn parse_type() -> impl Parser<char, Type, Error = Simple<char>> + Clone {
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

// <comp-task-def> ::= (:task <task-def>)
pub fn parse_comptaskdef() -> impl Parser<char, TaskDef, Error = Simple<char>> + Clone {
    parse_taskdef().delimited_by(just('(').then(just(":task").padded()), just(')').padded())
}

// <task-def> ::= <task-symbol>
//     :parameters (<typed list (variable)>)
pub fn parse_taskdef() -> impl Parser<char, TaskDef, Error = Simple<char>> + Clone {
    parse_tasksymbol()
        .then(parse_typedlist(parse_variable()).delimited_by(
            just(":parameters").padded().then(just('(').padded()),
            just(')').padded(),
        ))
        .map(|(symbol, parameters)| TaskDef { symbol, parameters })
}

// <task-symbol> ::= <name>
pub fn parse_tasksymbol() -> impl Parser<char, TaskSymbol, Error = Simple<char>> + Clone {
    parse_name().map(|name| TaskSymbol { name })
}

// <subtask-def> ::= (<task-symbol> <term>*)
//     | (<subtask-id> (<task-symbol> <term>*))
pub fn parse_subtaskdef() -> impl Parser<char, SubtaskDef, Error = Simple<char>> + Clone {
    let parse_inner = || {
        parse_tasksymbol()
            .then(parse_term().padded().repeated())
            .delimited_by(just('(').padded(), just(')').padded())
    };

    parse_inner()
        .map(|(symbol, terms)| SubtaskDef {
            symbol,
            terms,
            name: None,
        })
        .or(parse_subtaskid()
            .then(parse_inner())
            .delimited_by(just('(').padded(), just(')').padded())
            .map(|(name, (symbol, terms))| SubtaskDef {
                symbol,
                terms,
                name: Some(name),
            }))
}

// <subtask-id> ::= <name>
pub fn parse_subtaskid() -> impl Parser<char, SubtaskId, Error = Simple<char>> + Clone {
    parse_name().map(|name| SubtaskId { name })
}

// <ordering-def> ::=
//     (<subtask-id> "<" <subtask-id>)
pub fn parse_orderingdef() -> impl Parser<char, OrderingDef, Error = Simple<char>> + Clone {
    parse_subtaskid()
        .separated_by(just('<').padded())
        .exactly(2)
        .delimited_by(just('(').padded(), just(')').padded())
        .map(|res| {
            let mut iter = res.into_iter();
            OrderingDef {
                lesser: iter.next().unwrap(),
                greater: iter.next().unwrap(),
            }
        })
}

pub fn parse_defs<T>(
    parser: impl Parser<char, T, Error = Simple<char>> + Clone,
) -> impl Parser<char, Vec<T>, Error = Simple<char>> + Clone {
    just('(')
        .padded()
        .then(just(')'))
        .map(|_| vec![])
        .or(parser.clone().map(|def| vec![def]))
        .or(parser
            .clone()
            .padded()
            .repeated()
            .at_least(1)
            .delimited_by(just('(').padded().then(just("and")), just(')')))
}

// <constraint-def> ::= ()
//     | (not (= <term> <term>))
//     | (= <term> <term>)
pub fn parse_constraintdef() -> impl Parser<char, ConstraintDef, Error = Simple<char>> + Clone {
    let eq = || {
        parse_term()
            .separated_by(whitespace())
            .exactly(2)
            .delimited_by(just('(').then(just('=').padded()), just(')').padded())
    };

    just('(')
        .padded()
        .then(just(')'))
        .map(|_| ConstraintDef::Empty)
        .or(eq().map(|terms| {
            let mut iter = terms.into_iter();
            ConstraintDef::Eq(iter.next().unwrap(), iter.next().unwrap())
        }))
        .or(eq()
            .delimited_by(just('(').then(just("not").padded()), just(')').padded())
            .map(|terms| {
                let mut iter = terms.into_iter();
                ConstraintDef::Neq(iter.next().unwrap(), iter.next().unwrap())
            }))
}

// <gd> ::= ()
// <gd> ::= <atomic formula (term)>
// <gd> ::= <literal (term)>
//     only with feature :negative-preconditions
// <gd> ::= (and <gd>*)
// <gd> ::= (or <gd>*)
//     only with feature :disjunctive-preconditions
// <gd> ::= (not <gd>)
//     only with feature :disjunctive-preconditions
// <gd> ::= (imply <gd> <gd>)
//     only with feature :disjunctive-preconditions
// <gd> ::=
//     (exists (<typed list (variable)>*) <gd>)
//         only with feature :existential-preconditions
// <gd> ::=
//     (forall (<typed list (variable)>*) <gd>)
//         only with feature :universal-preconditions
// <gd> ::= (= <term> <term>)
pub fn parse_gd() -> impl Parser<char, GD, Error = Simple<char>> + Clone {
    recursive(|gd_parser| {
        // <gd> ::= ()
        just('(')
            .padded()
            .then(just(')'))
            .map(|_| GD::Empty)
            // <gd> ::= <atomic formula (term)>
            .or(parse_atomicformula(parse_term()).map(|formula| GD::Formula(formula)))
            // <gd> ::= <literal (term)>
            .or(parse_literal(parse_term()).map(|literal| GD::Literal(literal)))
            // <gd> ::= (and <gd>*)
            .or(gd_parser
                .clone()
                .padded()
                .repeated()
                .delimited_by(just('(').then(just("and").padded()), just(')'))
                .map(|gds| GD::And(gds)))
            // <gd> ::= (or <gd>*)
            .or(gd_parser
                .clone()
                .padded()
                .repeated()
                .delimited_by(just('(').then(just("or").padded()), just(')'))
                .map(|gds| GD::Or(gds)))
            // <gd> ::= (not <gd>)
            .or(gd_parser
                .clone()
                .delimited_by(just('(').then(just("not").padded()), just(')'))
                .map(|gd| GD::Not(Box::new(gd))))
            // <gd> ::= (imply <gd> <gd>)
            .or(gd_parser
                .clone()
                .then(gd_parser.clone().padded())
                .delimited_by(just('(').then(just("imply").padded()), just(')').padded())
                .map(|(gd_1, gd_2)| GD::Imply(Box::new(gd_1), Box::new(gd_2))))
            // <gd> ::=
            //     (exists (<typed list (variable)>*) <gd>)
            .or(parse_typedlist(parse_variable())
                .padded()
                .repeated()
                .delimited_by(just('('), just(')'))
                .then(gd_parser.clone())
                .delimited_by(just('(').then(just("exists").padded()), just(')').padded())
                .map(|(list, gd)| GD::Exists(list, Box::new(gd))))
            // <gd> ::=
            //     (forall (<typed list (variable)>*) <gd>)
            .or(parse_typedlist(parse_variable())
                .padded()
                .repeated()
                .delimited_by(just('('), just(')'))
                .then(gd_parser.clone())
                .delimited_by(just('(').then(just("forall").padded()), just(')').padded())
                .map(|(list, gd)| GD::ForAll(list, Box::new(gd))))
            // <gd> ::= (= <term> <term>)
            .or(parse_term()
                .then(parse_term().padded())
                .delimited_by(just('(').then(just('=').padded()), just(')'))
                .map(|(t_1, t_2)| GD::Eq(t_1, t_2)))
    })
}

// <literal (t)> ::= <atomic formula(t)>
// <literal (t)> ::= (not <atomic formula(t)>)
pub fn parse_literal<T>(
    parser: impl Parser<char, T, Error = Simple<char>> + Clone,
) -> impl Parser<char, Literal<T>, Error = Simple<char>> + Clone {
    parse_atomicformula(parser.clone())
        .map(|formula| Literal::Pos(formula))
        .or(parse_atomicformula(parser)
            .delimited_by(just('(').then(just("not").padded()), just(')').padded())
            .map(|formula| Literal::Neg(formula)))
}

// <atomic formula(t)> ::= (<predicate> t*)
pub fn parse_atomicformula<T>(
    parser: impl Parser<char, T, Error = Simple<char>> + Clone,
) -> impl Parser<char, AtomicFormula<T>, Error = Simple<char>> + Clone {
    parse_predicate()
        .then(parser.padded().repeated())
        .delimited_by(just('(').padded(), just(')').padded())
        .map(|(predicate, elems)| AtomicFormula { predicate, elems })
}

// <term> ::= <name>
pub fn parse_term() -> impl Parser<char, Term, Error = Simple<char>> + Clone {
    parse_name()
        .map(|name| Term::Name(name))
        .or(parse_variable().map(|var| Term::Var(var)))
}

// <p-effect> ::= (not <atomic formula(term)>)
// <p-effect> ::= <atomic formula(term)>
pub fn parse_peffect() -> impl Parser<char, PEffect, Error = Simple<char>> + Clone {
    parse_atomicformula(parse_term())
        .map(|formula| PEffect::Pos(formula))
        .or(parse_atomicformula(parse_term())
            .delimited_by(just('(').then(just("not").padded()), just(')').padded())
            .map(|formula| PEffect::Neg(formula)))
}

// <cond-effect> ::= (and <p-effect>*)
// <cond-effect> ::= <p-effect>
pub fn parse_condeffect() -> impl Parser<char, Vec<PEffect>, Error = Simple<char>> {
    parse_peffect()
        .padded()
        .repeated()
        .delimited_by(just('(').then(just("and").padded()), just(')').padded())
        .or(parse_peffect().map(|peffect| vec![peffect]))
}
