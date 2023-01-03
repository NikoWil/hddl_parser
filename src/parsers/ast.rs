// <base-type> ::= <name>
pub struct BaseType {
    pub name: String,
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

// <task-symbol> ::= <name>
pub struct TaskSymbol {
    pub name: String,
}

// <subtask-id> ::= <name>
pub struct SubtaskId {
    pub name: String,
}

// <term> ::= <name>
pub struct Term {
    pub name: String,
}
