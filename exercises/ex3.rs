use std::collections::HashMap;
/// Use a type-alias to declare that when we write `Id`, we actually mean
/// `String`.
type Id = String;

/// An expression in a simple language with arithmetic and variables. An
/// expression is any of the following:
///
/// Literal, e.g., 3 (u32)
/// Variable, e.g., x (Id)
/// Assignment, e.g., x = y + 2
/// Application of a binary operator, e.g., x + 3
/// A sequence of two expressions, e.g., x = 1; x + 3
enum Expr {
    Lit(u32), // TODO add missing constructors
    Var(Id),
    Asgn(Id,Box<Expr>),
    App(BinOp,Box<Expr>,Box<Expr),
    Seq(Box<Expr>,Box<Expr>),
}

enum BinOp{
    Add,
    Sub,
    Mul,
}

use self::BinOp::*;

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

/// The environment maps `Id`s to `u32`s.
struct Env {
    // TODO replace u32 with the right type
   vars:HashMap<Id,u32>,
}

impl Env {
    /// Create a new empty environment.
    fn new()-> Env{
        Env{
            vars:HashMap::new(),
        }

    }
    fn lookup(&self,id:&Id) -> Option<u32>{
        self.vars.get(id).cloned()
    }
    fn assign(&mut self,id:Id,val:u32){
        self.vars.insert(id,val);
    }
}

impl Expr {
    /// Evaluate an expression using the given environment to look up and
    /// assign variables in. An `Err` can only occur when a variable is used
    /// before it is assigned.
    fn eval(&self, env: &mut Env) -> Result<u32, String> {
        match self{
            Lit(u)=>Ok(*u),
            Var(id)=>env.lookup.ok_or(format!("undefined variable: {}",id)),
            Asgn(id,expr) =>{
                let res = expr.eval(env)?;
                env.assign(id.to_owned(),res);
                Ok(res)
            }
            App(op,expr1,expr2){
                let res1 = expr1.eval(env)?;
                let res2 = expr2.eval(env)?;
                Ok(match op{
                    Add => res1 +res2,
                    Sub => res1 -res2,
                    Mul => res1 * res2,
                })

            }
            Seq(expr1,expr2)=>{
                expr1.eval(env)?;
                expr2.eval
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
    // Try out some examples by replacing the ? with an expression
    // match ?.eval_() {
    //     Ok(n)  => println!("Ok: {}", n),
    //     Err(e) => println!("Err: {}", e)
    // }
    match Seq(
        Box::new(Asgn("x".to_owned(),Box::new(Lit(1)))),
        Box::new(Var("x".to_owned())),
    )
    .eval_()
    {
        Ok(n)=>println!("Ok:{}",n),
        Err(e)=>println!("Err:{}",e),
    }
}
