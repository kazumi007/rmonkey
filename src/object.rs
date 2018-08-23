use ast::{Expression, Statement};
use fnv::FnvHashMap;
use std::cell::RefCell;
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
    HashMap(FnvHashMap<Hashable, Rc<MObject>>),
    ReturnValue(Rc<MObject>),
    Function {
        params: Vec<Expression>,
        body: Box<Statement>,
        env: Rc<RefCell<Environment>>,
    },
    BuiltinFunc {
        name: String,
    },
    Quote(Box<Expression>),
    Macro {
        params: Vec<Expression>, // Expression::Identifier
        body: Box<Statement>,    // Statement::BlockStatement
        env: Rc<RefCell<Environment>>,
    },
}

fn join(vals : &[Rc<MObject>], separator:&str)->String {
    let mut s = String::new();
    for (i, val) in vals.iter().enumerate() {
        if i != 0 {
            s += separator;
         }
         s += &val.to_string();
    }
    s
}

impl fmt::Display for MObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MObject::Int(val) => write!(f, "{}", val),
            MObject::Bool(val) => write!(f, "{}", val),
            MObject::Null => write!(f, "nil"),
            MObject::Str(val) => write!(f, "\"{}\"", val),
            MObject::Array(vals) => {
                write!(f, "[{}]", join(&vals, ", "))
            }
            MObject::ReturnValue(val) => write!(f, "{}", val),
            MObject::Function { params, body, .. } => {
                write!(f, "fn(");
                for (i, param) in params.into_iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ");
                    }
                    write!(f, "{}", param);
                }
                write!(f, "){{");
                write!(f, "{}", body);
                write!(f, "}}")
            }
            MObject::BuiltinFunc { name } => write!(f, "{}", name),
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
            MObject::Quote(node) => write!(f, "quote({})", node),
            MObject::Macro { .. } => {
                write!(f, "macro(");
                write!(f, "...)")
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: FnvHashMap<String, Rc<MObject>>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: FnvHashMap::default(),
            parent: None,
        }
    }

    pub fn get(&self, key: &str) -> Option<Rc<MObject>> {
        match self.store.get(key) {
            Some(v) => Some(v.clone()),
            None => match self.parent {
                Some(ref v) => v.borrow().get(key),
                _ => None,
            },
        }
    }

    pub fn put(&mut self, key: &str, obj: &Rc<MObject>) {
        self.store.insert(key.to_string(), obj.clone());
    }

    pub fn enclose_env(outer: &Rc<RefCell<Environment>>) -> Environment {
        Environment {
            store: FnvHashMap::default(),
            parent: Some(outer.clone()),
        }
    }
}
