// <types-def> ::= (:types <types>+)
pub struct TypesDef {
    pub types: Vec<Types>,
}

// <types> ::= <typed list (name)>
//     | <base-type>
pub enum Types {
    List(TypedList<String>),
    Base(BaseType),
}

// <base-type> ::= <name>
pub struct BaseType {
    pub name: String,
}

// <constants-def> ::=
//     (:constants <typed list (name)>)
pub struct ConstantsDef {
    pub constants: TypedList<String>,
}

// <predicates-def> ::=
//     (:predicates <atomic-formula-skeleton>+)
pub struct PredicatesDef {
    pub predicates: Vec<AtomicFormulaSkeleton>,
}

// <atomic-formula-skeleton> ::=
//     (<predicate> <typed list (variable)>)
pub struct AtomicFormulaSkeleton {
    pub predicate: Predicate,
    pub typed_list: TypedList<Variable>,
}

// <predicate> ::= <name>
pub struct Predicate {
    pub name: String,
}

// <variable> ::= ?<name>
pub struct Variable {
    pub name: String,
}

// <typed list (x)> ::= x+ - <type>
//     [<typed list (x)>]
pub struct TypedList<T> {
    pub elems: Vec<(Vec<T>, Type)>,
}

// <primitive-type> ::= <name>
pub struct PrimitiveType {
    pub name: String,
}

// <type> ::= (either <primitive-type>+)
// <type> ::= <primitive-type>
pub enum Type {
    Simple(PrimitiveType),
    Either(Vec<PrimitiveType>),
}

// <task-def> ::= <task-symbol>
//     :parameters (<typed list (variable)>)
pub struct TaskDef {
    pub symbol: TaskSymbol,
    pub parameters: TypedList<Variable>,
}

// <task-symbol> ::= <name>
pub struct TaskSymbol {
    pub name: String,
}

// <subtask-def> ::= (<task-symbol> <term>*)
//     | (<subtask-id> (<task-symbol> <term>*))
pub struct SubtaskDef {
    pub symbol: TaskSymbol,
    pub terms: Vec<Term>,
    pub name: Option<SubtaskId>,
}

// <subtask-id> ::= <name>
pub struct SubtaskId {
    pub name: String,
}

// <ordering-def> ::=
//     (<subtask-id> "<" <subtask-id>)
pub struct OrderingDef {
    pub lesser: SubtaskId,
    pub greater: SubtaskId,
}

// <constraint-def> ::= ()
//     | (not (= <term> <term>))
//     | (= <term> <term>)
pub enum ConstraintDef {
    Empty,
    Neq(Term, Term),
    Eq(Term, Term),
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
pub enum GD {
    Empty,
    Formula(AtomicFormula<Term>),
    Literal(Literal<Term>),
    And(Vec<GD>),
    Or(Vec<GD>),
    Not(Box<GD>),
    Imply(Box<GD>, Box<GD>),
    Exists(Vec<TypedList<Variable>>, Box<GD>),
    ForAll(Vec<TypedList<Variable>>, Box<GD>),
    Eq(Term, Term),
}

// <literal (t)> ::= <atomic formula(t)>
// <literal (t)> ::= (not <atomic formula(t)>)
pub enum Literal<T> {
    Pos(AtomicFormula<T>),
    Neg(AtomicFormula<T>),
}

// <atomic formula(t)> ::= (<predicate> t*)
pub struct AtomicFormula<T> {
    pub predicate: Predicate,
    pub elems: Vec<T>,
}

// <term> ::= <name>
pub enum Term {
    Name(String),
    Var(Variable),
}

// <effect> ::= ()
// <effect> ::= (and <c-effect>*)
// <effect> ::= <c-effect>
pub struct Effect {
    c_effects: Vec<CEffect>,
}

// <c-effect> ::=
//         only with feature :conditional-effects
//     (forall (<variable>*) <effect>)
// <c-effect> ::=
//     (when <gd> <cond-effect>)
//         only with feature :conditional-effects
// <c-effect> ::= <p-effect>
pub enum CEffect {
    Single(PEffect),
    ForAll(Vec<Variable>, Box<Effect>),
}

// <p-effect> ::= (not <atomic formula(term)>)
// <p-effect> ::= <atomic formula(term)>
pub enum PEffect {
    Pos(AtomicFormula<Term>),
    Neg(AtomicFormula<Term>),
}
