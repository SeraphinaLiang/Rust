use std::collections::HashMap;

/// Use a type-alias to declare that when we write `Id`, we actually mean
/// `String`.
type Id = String;

/// An expression in a simple language with arithmetic and variables.
enum Expr {
    /// Literal, e.g., 3 (u32)
    Lit(u32),
    /// Variable, e.g., x (Id)
    Var(Id),
    /// Assignment, e.g., x = y + 2
    Asgn(Id, Box<Expr>),
    /// Application of a binary operator, e.g., x + 3
    App(BinOp, Box<Expr>, Box<Expr>),
    /// A sequence of two expressions, e.g., x = 1; x + 3
    Seq(Box<Expr>, Box<Expr>),
}

// Bring the constructors of `Expr` in scope, see
// https://doc.rust-lang.org/rust-by-example/custom_types/enum/enum_use.html
use self::Expr::*;
// With this we can write:
//
//     match e {
//         Lit(n) => ...
//         ...
//     }
//
// Without this, we would have to write:
//
//     match e {
//         Expr::Lit(n) => ...
//         ...
//     }

/// The different binary operations: +, -, and *.
enum BinOp {
    Add,
    Sub,
    Mul,
}

// Bring the constructors of `BinOp` in scope.
use self::BinOp::*;

/// The environment maps `Id`s to `u32`s.
struct Env {
    vars: HashMap<Id, u32>,
}

impl Env {
    /// Create a new empty environment.
    fn new() -> Env {
        Env {
            vars: HashMap::new(),
        }
    }
    /// Look up `id` in the environment.
    fn lookup(&self, id: &Id) -> Option<u32> {
        self.vars.get(id).cloned()
        // OR:
        // self.vars.get(id).map(|x| *x)
    }
    /// Map `id` to `val` in the environment.
    fn assign(&mut self, id: Id, val: u32) {
        self.vars.insert(id, val);
    }
}

impl Expr {
    /// Evaluate an expression using the given environment to look up and
    /// assign variables in. An `Err` can only occur when a variable is used
    /// before it is assigned.
    fn eval(&self, env: &mut Env) -> Result<u32, String> {
        match self {
            Lit(u) => Ok(*u),
            Var(id) => env.lookup(id).ok_or(format!("undefined variable: {}", id)),
            Asgn(id, expr) => {
                let res = expr.eval(env)?;
                env.assign(id.to_owned(), res);
                Ok(res)
            }
            App(op, expr1, expr2) => {
                let res1 = expr1.eval(env)?;
                let res2 = expr2.eval(env)?;
                Ok(match op {
                    Add => res1 + res2,
                    Sub => res1 - res2,
                    Mul => res1 * res2,
                })
            }
            Seq(expr1, expr2) => {
                expr1.eval(env)?;
                expr2.eval(env)
            }
        }
    }

    /// Evaluate an expression using an empty environment.
    fn eval_(&self) -> Result<u32, String> {
        let mut env = Env::new();
        self.eval(&mut env)
    }
}

fn main() {
    // "x = 1; x"
    match Seq(
        Box::new(Asgn("x".to_owned(), Box::new(Lit(1)))),
        Box::new(Var("x".to_owned())),
    )
    .eval_()
    {
        Ok(n) => println!("Ok: {}", n),
        Err(e) => println!("Err: {}", e),
    }
}

#[test]
fn tests() {
    assert_eq!(
        Ok(1),
        Seq(
            Box::new(Asgn("x".to_owned(), Box::new(Lit(1)))),
            Box::new(Var("x".to_owned()))
        )
        .eval_()
    );
    assert_eq!(
        Ok(15),
        Seq(
            Box::new(Asgn("x".to_owned(), Box::new(Lit(1)))),
            Box::new(Seq(
                Box::new(Asgn("y".to_owned(), Box::new(Lit(14)))),
                Box::new(App(
                    Add,
                    Box::new(Var("x".to_owned())),
                    Box::new(Var("y".to_owned()))
                ))
            ))
        )
        .eval_()
    );
    assert_eq!(
        Err("undefined variable: x".to_owned()),
        Var("x".to_owned()).eval_()
    );
}
