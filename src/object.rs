use ast::{Expression, Statement};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Hashable {
    Int(i64),
    Bool(bool),
    Str(String),
}

impl Hashable {
    pub fn from_str(s: &str) -> Hashable {
        Hashable::Str(s.to_string())
    }

    pub fn from(o: &MObject) -> Result<Hashable, String> {
        match o {
            MObject::Int(v) => Ok(Hashable::Int(*v)),
            MObject::Bool(v) => Ok(Hashable::Bool(*v)),
            MObject::Str(v) => Ok(Hashable::Str(v.clone())),
            _ => Err(format!("unsupported hash key object: {}", o).to_string()),
        }
    }
}

impl fmt::Display for Hashable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Hashable::Int(val) => write!(f, "{}", val),
            Hashable::Bool(val) => write!(f, "{}", val),
            Hashable::Str(val) => write!(f, "\"{}\"", val),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum MObject {
    Int(i64),
    Bool(bool),
    Null,
    Str(String),
    Array(Vec<Rc<MObject>>),
    HashMap(HashMap<Hashable, Rc<MObject>>),
    ReturnValue(Rc<MObject>),
    Function {
        params: Vec<Expression>,
        body: Box<Statement>,
        env: Rc<RefCell<Environment>>,
    },
    BuiltinFunc {
        name: String,
    },
}

impl fmt::Display for MObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MObject::Int(val) => write!(f, "{}", val),
            MObject::Bool(val) => write!(f, "{}", val),
            MObject::Null => write!(f, "nil"),
            MObject::Str(val) => write!(f, "\"{}\"", val),
            MObject::Array(vals) => {
                write!(f, "[");
                let mut is_first = true;
                for val in vals {
                    if !is_first {
                        write!(f, ", ");
                    }
                    write!(f, "{}", val);
                    is_first = false;
                }
                write!(f, "]")
            }
            MObject::ReturnValue(val) => write!(f, "ReturnValue{}", val),
            MObject::Function { .. } => write!(f, "Function"),
            MObject::BuiltinFunc { name } => write!(f, "BuiltinFunc{}", name),
            MObject::HashMap(ref map) => {
                write!(f, "{{");
                let mut is_first = true;
                for (key, val) in map {
                    if !is_first {
                        write!(f, ", ");
                    }
                    write!(f, "{} : {}", key, val);
                    is_first = false;
                }
                write!(f, "}}")
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Environment {
    Root(HashMap<String, Rc<MObject>>),
    Store(HashMap<String, Rc<MObject>>, Rc<RefCell<Environment>>),
}

impl Environment {
    pub fn new() -> Environment {
        Environment::Root(HashMap::new())
    }

    pub fn get(&self, key: &str) -> Option<Rc<MObject>> {
        match self {
            Environment::Root(ref inner) => inner.get(key).cloned(),
            Environment::Store(ref inner, ref outer) => match inner.get(key) {
                Some(v) => Some(v.clone()),
                None => outer.borrow_mut().get(key),
            },
        }
    }

    pub fn put(&mut self, key: &str, obj: &Rc<MObject>) {
        match self {
            Environment::Root(ref mut env) => env.insert(key.to_string(), obj.clone()),
            Environment::Store(ref mut env, _) => env.insert(key.to_string(), obj.clone()),
        };
    }

    pub fn enclose_env(outer: &Rc<RefCell<Environment>>) -> Environment {
        Environment::Store(HashMap::new(), outer.clone())
    }
}
