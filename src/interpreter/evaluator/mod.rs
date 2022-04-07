pub mod builtin;
pub mod environment;
pub mod errors;
mod object;
mod procedure;

use crate::interpreter::analysis::ast::*;
use crate::interpreter::evaluator::errors::{RuntimeError, RuntimeResult};
use crate::interpreter::evaluator::object::Object;
use crate::interpreter::evaluator::procedure::Procedure;
use environment::{EnvIdent, Environment};

use std::collections::HashMap;
use std::io::Write;

pub struct Evaluator<'a> {
    env: Environment<'a>,
    program: &'a Program,
    procedures: &'a HashMap<&'a str, Procedure>,
}

impl<'a> Evaluator<'a> {
    pub fn get_procedures(
        program: &'a Program,
        include_builtin: bool,
    ) -> RuntimeResult<HashMap<&'a str, Procedure>> {
        let mut procedures = HashMap::new();
        if include_builtin {
            procedures.extend(builtin::new_builtin_procedures());
        }
        for statement in program {
            match statement {
                Statement::Procedure(proc) => {
                    procedures.insert(&*proc.ident.value, Procedure::UserDefined(proc.clone()))
                }
                _ => return Err(RuntimeError::NonProcedureInTopLevel),
            };
        }
        Ok(procedures)
    }

    pub fn new(
        env: Environment<'a>,
        program: &'a Program,
        procedures: &'a HashMap<&'a str, Procedure>,
    ) -> RuntimeResult<Self> {
        Ok(Self {
            env,
            program,
            procedures,
        })
    }

    pub fn eval(&mut self) -> RuntimeResult<()> {
        for statement in self.program {
            if let Statement::Procedure(proc) = statement {
                if proc.ident.value == "principal" {
                    let result = self.eval_block_statement(&proc.block);
                    if let Err(err) = result {
                        println!("{}", err);
                        std::io::stdout().flush().unwrap();
                        std::process::exit(0);
                    }
                    break;
                }
            }
        }
        Ok(())
    }

    fn eval_block_statement(&mut self, block: &'a [Statement]) -> RuntimeResult<()> {
        self.env.enter_scope();
        for statement in block {
            self.eval_statement(statement)?;
        }
        self.env.exit_scope();
        Ok(())
    }

    fn eval_statement(&mut self, statement: &'a Statement) -> RuntimeResult<()> {
        match statement {
            Statement::Assignment(assignment) => self.eval_assignment_statement(&assignment)?,
            Statement::If(if_statement) => self.eval_if_statement(&if_statement)?,
            Statement::Loop(loop_statement) => self.eval_loop_statement(&loop_statement)?,
            Statement::Expression(expr_statement) => {
                self.eval_expression_statement(&expr_statement)?
            }
            _ => {}
        }
        Ok(())
    }

    fn eval_assignment_statement(
        &mut self,
        assignment: &'a AssignmentStatement,
    ) -> RuntimeResult<()> {
        let ident_str = &assignment.ident.value;
        let value = self.eval_expr(&assignment.value)?;
        // TODO: Oh god, do it in another way
        if let Some(indices) = &assignment.index {
            let index = self.indices_to_string(&indices)?;
            let ident_str = format!("%{}{}", ident_str, index);
            self.env.set(EnvIdent::Owned(ident_str), value);
        } else {
            self.env.set(EnvIdent::Borrowed(ident_str), value);
        }
        Ok(())
    }

    fn eval_if_statement(&mut self, if_statement: &'a IfStatement) -> RuntimeResult<()> {
        let cond_expr = self.eval_expr(&if_statement.condition)?;
        match cond_expr {
            Object::Bool(boolean) => {
                if boolean {
                    self.eval_block_statement(&if_statement.consequence)?
                } else if let Some(alternative) = &if_statement.alternative {
                    self.eval_block_statement(alternative)?;
                }
            }
            _ => return Err(RuntimeError::IfConditionNotABool),
        }
        Ok(())
    }

    fn eval_loop_statement(&mut self, loop_statement: &'a LoopStatement) -> RuntimeResult<()> {
        self.env.enter_scope();
        let block = &loop_statement.block;
        'loop_statement: loop {
            for statement in block {
                match statement {
                    Statement::BreakIf(break_if_statement) => {
                        let condition = self.eval_expr(&break_if_statement.condition)?;
                        match condition {
                            Object::Bool(v) => {
                                if v {
                                    break 'loop_statement;
                                }
                            }
                            _ => return Err(RuntimeError::BreakIfConditionNotABool),
                        }
                    }
                    statement => self.eval_statement(statement)?,
                }
            }
        }
        self.env.exit_scope();
        Ok(())
    }

    fn eval_expression_statement(&mut self, expr: &'a ExpressionStatement) -> RuntimeResult<()> {
        match expr {
            ExpressionStatement::Call(call_expr) => self.eval_call_expr(call_expr)?,
        };
        Ok(())
    }

    fn eval_expr(&mut self, expr: &'a Expression) -> RuntimeResult<Object> {
        match expr {
            Expression::Ident(ident_expr) => self.eval_ident_expr(ident_expr),
            Expression::FloatLiteral(lit) => Ok(Object::Float(*lit)),
            Expression::StringLiteral(string) => Ok(Object::String(string.clone())),
            Expression::BoolLiteral(boolean) => Ok(Object::Bool(*boolean)),
            Expression::Call(call_expr) => {
                if let Some(out_value) = self.eval_call_expr(call_expr)? {
                    Ok(out_value)
                } else {
                    Err(RuntimeError::ProcedureCallExprNotValid)
                }
            }
            Expression::Index(index_expr) => self.eval_index_expr(index_expr),
            Expression::Infix(infix) => {
                let left = self.eval_expr(&*infix.left)?;
                let right = self.eval_expr(&*infix.right)?;
                self.eval_infix_expr(infix.kind, &left, &right)
            }
            Expression::Prefix(prefix) => {
                let right = self.eval_expr(&*prefix.right)?;
                self.eval_prefix_expr(prefix.kind, &right)
            }
        }
    }

    fn eval_infix_expr(
        &mut self,
        kind: InfixKind,
        left: &Object,
        right: &Object,
    ) -> RuntimeResult<Object> {
        // TODO: Handle string concatenation with non-string types (implicit conversion)
        match left {
            Object::Float(lv) => match right {
                Object::Float(rv) => self.eval_infix_float_expr(kind, *lv, *rv),
                _ => Err(RuntimeError::InfixOpWithInvalidTypes),
            },
            Object::String(lv) => match right {
                Object::String(rv) => self.eval_infix_string_expr(kind, lv, rv),
                _ => Err(RuntimeError::InfixOpWithInvalidTypes),
            },
            Object::Bool(lv) => match right {
                Object::Bool(rv) => self.eval_infix_bool_expr(kind, *lv, *rv),
                _ => Err(RuntimeError::InfixOpWithInvalidTypes),
            },
            _ => Err(RuntimeError::InfixOpWithInvalidTypes),
        }
    }

    fn eval_infix_float_expr(
        &mut self,
        kind: InfixKind,
        left: f64,
        right: f64,
    ) -> RuntimeResult<Object> {
        match kind {
            InfixKind::Plus => Ok(Object::Float(left + right)),
            InfixKind::Minus => Ok(Object::Float(left - right)),
            InfixKind::Multiply => Ok(Object::Float(left * right)),
            InfixKind::Divide => Ok(Object::Float(left / right)),
            InfixKind::Power => Ok(Object::Float(left.powf(right))),
            InfixKind::Modulo => Ok(Object::Float(left % right)),
            InfixKind::Equal => Ok(Object::Bool((left - right).abs() <= std::f64::EPSILON)),
            InfixKind::NotEqual => Ok(Object::Bool((left - right).abs() > std::f64::EPSILON)),
            InfixKind::LessThan => Ok(Object::Bool(left < right)),
            InfixKind::LessThanEqual => Ok(Object::Bool(left <= right)),
            InfixKind::GreaterThan => Ok(Object::Bool(left > right)),
            InfixKind::GreaterThanEqual => Ok(Object::Bool(left >= right)),
            _ => Err(RuntimeError::InfixOpWithInvalidTypes),
        }
    }

    fn eval_infix_string_expr(
        &mut self,
        kind: InfixKind,
        left: &str,
        right: &str,
    ) -> RuntimeResult<Object> {
        match kind {
            InfixKind::Plus => Ok(Object::String(format!("{}{}", left, right))),
            _ => Err(RuntimeError::InfixOpWithInvalidTypes),
        }
    }

    fn eval_infix_bool_expr(
        &mut self,
        kind: InfixKind,
        lv: bool,
        rv: bool,
    ) -> RuntimeResult<Object> {
        match kind {
            InfixKind::Or => Ok(Object::Bool(lv || rv)),
            InfixKind::And => Ok(Object::Bool(lv && rv)),
            _ => Err(RuntimeError::InfixOpWithInvalidTypes),
        }
    }

    fn eval_prefix_expr(&mut self, kind: PrefixKind, right: &Object) -> RuntimeResult<Object> {
        match right {
            Object::Bool(v) => match kind {
                PrefixKind::Not => Ok(Object::Bool(!*v)),
                _ => Err(RuntimeError::PrefixOpWithInvalidType),
            },
            Object::Float(v) => match kind {
                PrefixKind::Plus => Ok(Object::Float(*v)),
                PrefixKind::Minus => Ok(Object::Float(-*v)),
                _ => Err(RuntimeError::PrefixOpWithInvalidType),
            },
            _ => Err(RuntimeError::PrefixOpWithInvalidType),
        }
    }

    fn eval_index_expr(&mut self, index_expr: &'a IndexExpression) -> RuntimeResult<Object> {
        let ident_string = &index_expr.ident.value;
        let indices = &index_expr.indices;

        let indices_string = self.indices_to_string(&indices)?;

        let ident_string = format!("%{}{}", ident_string, indices_string);
        self.env.get(&EnvIdent::Owned(ident_string))
    }

    fn eval_call_expr(&mut self, call_expr: &'a CallExpression) -> RuntimeResult<Option<Object>> {
        let proc_ident_str = &call_expr.procedure_ident.value;
        let proc = self.procedures.get::<str>(&proc_ident_str).unwrap();
        let proc_params = match proc {
            Procedure::UserDefined(ref user_proc) => &user_proc.parameters,
            Procedure::Builtin(ref builtin_proc) => &builtin_proc.parameters,
        };

        let call_object_args = self.eval_arguments(&call_expr.arguments, &proc_params)?;

        let mut out_values = self.eval_procedure(&proc, &call_object_args)?;

        let call_expr_args = &call_expr.arguments;
        let params_args = proc_params.iter().zip(call_expr_args.iter());

        let mut i: usize = 0;
        for param_arg in params_args {
            if param_arg.0.is_out {
                match param_arg.1 {
                    Expression::Ident(ident_expr) => {
                        let out_value = out_values.remove(i);
                        self.env
                            .set(EnvIdent::Borrowed(&ident_expr.value), out_value)
                    }
                    _ => return Err(RuntimeError::OutParamNoIdent),
                }
                i += 1;
            }
        }
        if out_values.len() == 1 {
            Ok(Some(out_values.remove(0)))
        } else {
            Ok(None)
        }
    }

    fn eval_procedure(
        &mut self,
        proc: &'a Procedure,
        args: &[Object],
    ) -> RuntimeResult<Vec<Object>> {
        self.env.enter_call_stack();
        let mut out_values: Vec<Object> = vec![];
        match proc {
            Procedure::UserDefined(user_proc) => {
                // bind the arguments to parameters' name
                let params_args = user_proc.parameters.iter().zip(args.iter());
                for param_arg in params_args {
                    let param_ident_string = &param_arg.0.ident.value;
                    let arg_value = param_arg.1.clone();
                    self.env
                        .set(EnvIdent::Borrowed(param_ident_string), arg_value);
                }

                // execute the procedure
                for statement in &user_proc.block {
                    self.eval_statement(statement)?;
                }

                // return the values of parameters with the «out» attribute
                for param in &user_proc.parameters {
                    if param.is_out {
                        out_values.push(self.env.get(&EnvIdent::Borrowed(&param.ident.value))?);
                    }
                }
            }
            Procedure::Builtin(builtin_proc) => {
                out_values.push((builtin_proc.proc_fn)(&args)?);
            }
        }
        self.env.exit_call_stack();
        Ok(out_values)
    }

    fn eval_ident_expr(&mut self, ident_expr: &'a IdentExpression) -> RuntimeResult<Object> {
        self.env.get(&EnvIdent::Borrowed(&ident_expr.value))
    }

    fn eval_arguments(
        &mut self,
        args: &'a [Expression],
        params: &[Parameter],
    ) -> RuntimeResult<Vec<Object>> {
        let mut obj_args: Vec<Object> = vec![];

        let args_params = args.iter().zip(params);

        for arg_param in args_params {
            if arg_param.1.is_in {
                let arg = self.eval_expr(arg_param.0)?;
                obj_args.push(arg);
            } else if arg_param.1.is_out {
                match arg_param.0 {
                    Expression::Ident(ident_expr) => {
                        self.env
                            .set(EnvIdent::Borrowed(&ident_expr.value), Object::None);
                        obj_args.push(Object::None);
                    }
                    _ => return Err(RuntimeError::OutParamNoIdent),
                }
            }
        }
        Ok(obj_args)
    }

    fn indices_to_string(&mut self, indices: &'a [Expression]) -> RuntimeResult<String> {
        let mut index_str = String::new();
        for index_expr in indices {
            let index_obj = self.eval_expr(&index_expr)?; // get index object
            let index = match index_obj {
                // convert index object to integer
                Object::Float(index_f64) => index_f64 as u64,
                _ => return Err(RuntimeError::IndexNotANumber),
            };
            index_str = format!("{},{}", index_str, index);
        }
        Ok(index_str)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::analysis::lexer::Lexer;
    use crate::interpreter::analysis::parser::Parser;

    #[test]
    fn test_simple() {
        let program_ast = Parser::new(Lexer::new(
            r#"
                    procedure main() {
                        fibonacci(3, result);
                        output(result, true);
                    }

                    procedure fibonacci(n, out result) {
                        if n = 0 || n = 1 {
                            result = n;
                        } else {
                            fibonacci(n-1, result1);
                            fibonacci(n-2, result2);
                            result = result1 + result2;
                        }
                    }
                    "#,
        ))
        .parse()
        .expect("panicked at parser");
        let procedures = Evaluator::get_procedures(&program_ast, true).unwrap();
        Evaluator::new(Environment::new(), &program_ast, &procedures)
            .expect("could not create evaluator")
            .eval()
            .expect("panicked at evaluator");
    }
}
