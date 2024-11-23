use crate::ast::*;
use crate::ir_gen::scope::Scope;
use koopa::ir::Program;

pub trait Evaluate {
    fn eval<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<i32>;
}

impl Evaluate for Exp {
    fn eval<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<i32> {
        match self {
            Exp::LOrExp(lor_exp) => lor_exp.eval(program, scope),
        }
    }
}

impl Evaluate for LOrExp {
    fn eval<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<i32> {
        match self {
            LOrExp::LAndExp(land_exp) => land_exp.eval(program, scope),
            #[rustfmt::skip]
            LOrExp::LOrExp(lor_exp, land_exp) => {
                let a = lor_exp.eval(program, scope)?;
                let b = land_exp.eval(program, scope)?;
                if a != 0 || b != 0 {Some(1)} else {Some(0)}
            }
        }
    }
}

impl Evaluate for LAndExp {
    fn eval<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<i32> {
        match self {
            LAndExp::EqExp(eq_exp) => eq_exp.eval(program, scope),
            #[rustfmt::skip]
            LAndExp::LAndExp(land_exp, eq_exp) => {
                let a = land_exp.eval(program, scope)?;
                let b = eq_exp.eval(program, scope)?;
                if a != 0 && b != 0 {Some(1)} else {Some(0)}
            }
        }
    }
}

impl Evaluate for EqExp {
    fn eval<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<i32> {
        match self {
            EqExp::RelExp(rel_exp) => rel_exp.eval(program, scope),
            #[rustfmt::skip]
            EqExp::EqExp(eq_exp, eq_op, rel_exp) => {
                let a = eq_exp.eval(program, scope)?;
                let b = rel_exp.eval(program, scope)?;
                match eq_op {
                    EqOp::Eq => if a == b {Some(1)} else {Some(0)},
                    EqOp::NotEq => if a != b {Some(1)} else {Some(0)},
                }
            }
        }
    }
}

impl Evaluate for RelExp {
    fn eval<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<i32> {
        match self {
            RelExp::AddExp(add_exp) => add_exp.eval(program, scope),
            #[rustfmt::skip]
            RelExp::RelExp(rel_exp, rel_op, add_exp) => {
                let a = rel_exp.eval(program, scope)?;
                let b = add_exp.eval(program, scope)?;
                match rel_op {
                    RelOp::Gt => if a > b {Some(1)} else {Some(0)},
                    RelOp::Lt => if a < b {Some(1)} else {Some(0)},
                    RelOp::Ge => if a >= b {Some(1)} else {Some(0)},
                    RelOp::Le => if a <= b {Some(1)} else {Some(0)},
                }
            }
        }
    }
}

impl Evaluate for AddExp {
    fn eval<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<i32> {
        match self {
            AddExp::MulExp(mul_exp) => mul_exp.eval(program, scope),
            #[rustfmt::skip]
            AddExp::AddAndMul(add_exp, add_op, mul_exp) => {
                let a = add_exp.eval(program, scope)?;
                let b = mul_exp.eval(program, scope)?;
                match add_op {
                    AddOp::Add => Some(a+b),
                    AddOp::Sub => Some(a-b),
                }
            }
        }
    }
}

impl Evaluate for MulExp {
    fn eval<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<i32> {
        match self {
            MulExp::UnaryExp(unary_exp) => unary_exp.eval(program, scope),
            #[rustfmt::skip]
            MulExp::MulAndUnary(mul_exp, mul_op, unary_exp) => {
                let a = mul_exp.eval(program, scope).unwrap();
                let b = unary_exp.eval(program, scope).unwrap();
                match mul_op {
                    MulOp::Mul => Some(a*b),
                    MulOp::Div => Some(a/b),
                    MulOp::Mod => Some(a%b),
                }
            }
        }
    }
}

impl Evaluate for UnaryExp {
    fn eval<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<i32> {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.eval(program, scope),
            #[rustfmt::skip]
            UnaryExp::UnaryOp(unary_op, unary_exp) => match unary_op {
                UnaryOp::Add => unary_exp.eval(program, scope),
                UnaryOp::Minus => Some(-unary_exp.eval(program, scope).unwrap()),
                UnaryOp::Not => {
                    if unary_exp.eval(program, scope)? == 0 {Some(1)} else {Some(0)}
                }
            },
            UnaryExp::Call(_) => {panic!("function call not implement compile time eval")}
        }
    }
}

impl Evaluate for PrimaryExp {
    fn eval<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<i32> {
        match self {
            PrimaryExp::Expression(exp) => exp.eval(program, scope),
            PrimaryExp::LVal(_) => None,
            PrimaryExp::Number(int) => Some(*int),
        }
    }
}
