use crate::low::environment::value::constant::ConstantDeclaration;
use crate::{
    low::environment::value::{function::FunctionDeclaration, variable::VariableDeclaration},
    make_id,
};

pub mod constant;
pub mod function;
pub mod variable;

make_id!(ValueId);

#[derive(Debug, Clone)]
pub enum ValueDeclaration {
    Variable(VariableDeclaration),
    Constant(ConstantDeclaration),
    Function(Box<FunctionDeclaration>),
}
