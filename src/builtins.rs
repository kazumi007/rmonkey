use object::MObject;
use std::collections::HashMap;
use std::rc::Rc;

pub type BuiltinFunc = fn(&[Rc<MObject>]) -> Result<Rc<MObject>, String>;

pub fn builtinmap() -> HashMap<String, BuiltinFunc> {
    let mut hm: HashMap<String, BuiltinFunc> = HashMap::new();
    hm.insert("len".to_string(), builtin_len);
    hm.insert("puts".to_string(), builtin_puts);
    hm.insert("first".to_string(), builtin_first);
    hm.insert("last".to_string(), builtin_last);
    hm.insert("rest".to_string(), builtin_rest);
    hm.insert("push".to_string(), builtin_push);
    hm
}

fn builtin_len(args: &[Rc<MObject>]) -> Result<Rc<MObject>, String> {
    if args.len() != 1 {
        return Err(format!("wrong number of arguments. got={} want=1", args.len()).to_string());
    }

    match &*args[0] {
        MObject::Str(ref value) => Ok(Rc::new(MObject::Int(value.len() as i64))),
        MObject::Array(ref values) => Ok(Rc::new(MObject::Int(values.len() as i64))),
        _ => Err(format!("argument to len not supported, {:?}", &*args[0]).to_string()),
    }
}

fn builtin_puts(args: &[Rc<MObject>]) -> Result<Rc<MObject>, String> {
    for arg in args {
        println!("{}", *arg);
    }
    Ok(Rc::new(MObject::Null))
}

fn builtin_first(args: &[Rc<MObject>]) -> Result<Rc<MObject>, String> {
    if args.len() != 1 {
        return Err(format!("wrong number of arguments. got={} want=1", args.len()).to_string());
    }

    match &*args[0] {
        MObject::Array(ref values) => values
            .first()
            .map_or_else(|| Ok(Rc::new(MObject::Null)), |v| Ok(v.clone())),
        _ => Ok(Rc::new(MObject::Null)),
    }
}

fn builtin_last(args: &[Rc<MObject>]) -> Result<Rc<MObject>, String> {
    if args.len() != 1 {
        return Err(format!("wrong number of arguments. got={} want=1", args.len()).to_string());
    }

    match &*args[0] {
        MObject::Array(ref values) => values
            .last()
            .map_or_else(|| Ok(Rc::new(MObject::Null)), |v| Ok(v.clone())),
        _ => Ok(Rc::new(MObject::Null)),
    }
}

fn builtin_rest(args: &[Rc<MObject>]) -> Result<Rc<MObject>, String> {
    if args.len() != 1 {
        return Err(format!("wrong number of arguments. got={} want=1", args.len()).to_string());
    }

    match &*args[0] {
        MObject::Array(ref values) => {
            if !values.is_empty() {
                let vec = values[1..].into_iter().cloned().collect();
                Ok(Rc::new(MObject::Array(vec)))
            } else {
                Ok(Rc::new(MObject::Null))
            }
        }
        _ => Ok(Rc::new(MObject::Null)),
    }
}

fn builtin_push(args: &[Rc<MObject>]) -> Result<Rc<MObject>, String> {
    if args.len() != 2 {
        return Err(format!("wrong number of arguments. got={} want=2", args.len()).to_string());
    }

    match &*args[0] {
        MObject::Array(ref values) => {
            let mut vec = values.into_iter().cloned().collect::<Vec<Rc<MObject>>>();
            vec.push(args[1].clone());
            Ok(Rc::new(MObject::Array(vec)))
        }
        _ => Err(format!("argument to `push` must be ARRAY, got {}", args[0]).to_string()),
    }
}
