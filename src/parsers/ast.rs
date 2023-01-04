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
