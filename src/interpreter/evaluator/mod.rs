pub mod builtin;
pub mod environment;
pub mod errors;
mod object;
mod procedure;

use crate::interpreter::analysis::ast::Precedence::Call;
use crate::interpreter::analysis::ast::*;
use crate::interpreter::analysis::token::Kind::Out;
use crate::interpreter::evaluator::errors::{RuntimeError, RuntimeResult};
use crate::interpreter::evaluator::object::Object;
use crate::interpreter::evaluator::procedure::Procedure;
use environment::Environment;

pub struct Evaluator {
    env: Environment,
}

impl Evaluator {
    pub fn new(env: Environment) -> Self {
        Self { env }
    }

    pub fn eval(&mut self, program: Program) -> RuntimeResult<()> {
        self.setup_env(&program)?;
        for statement in program {
            if let Statement::Procedure(proc) = statement {
                if proc.ident.value == "main" {
                    let result = self.eval_block_statement(&proc.block);
                    if let Err(err) = result {
                        match err {
                            RuntimeError::Generic(err) => {
                                println!("\nPROGRAM ERROR: {}", err);
                                std::process::exit(0);
                            }
                        }
                    }
                    break;
                }
            }
        }
        Ok(())
    }

    fn setup_env(&mut self, program: &[Statement]) -> RuntimeResult<()> {
        for statement in program {
            match statement {
                Statement::Procedure(proc) => self.env.add_procedure(
                    proc.ident.value.clone(),
                    Procedure::UserDefined(proc.clone()),
                ),
                _ => Err(RuntimeError::Generic(String::from(
                    "Tried to use a non-procedure statement in the top level",
                ))),
            }?;
        }
        Ok(())
    }

    fn eval_block_statement(&mut self, block: &[Statement]) -> RuntimeResult<()> {
        self.env.enter_scope();
        for statement in block {
            self.eval_statement(statement)?;
        }
        self.env.exit_scope();
        Ok(())
    }

    fn eval_statement(&mut self, statement: &Statement) -> RuntimeResult<()> {
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

    fn eval_assignment_statement(&mut self, assignment: &AssignmentStatement) -> RuntimeResult<()> {
        let mut ident_str = assignment.ident.value.clone();
        let value = self.eval_expr(&assignment.value)?;
        // TODO: Oh god, do it in another way
        if let Some(indices) = &assignment.index {
            let index = self.indices_to_string(&indices)?;

            ident_str = format!("%{}{}", ident_str, index);
        }
        self.env.set(ident_str.clone(), value);
        Ok(())
    }

    fn eval_if_statement(&mut self, if_statement: &IfStatement) -> RuntimeResult<()> {
        let cond_expr = self.eval_expr(&if_statement.condition)?;
        match cond_expr {
            Object::Bool(boolean) => {
                if boolean {
                    self.eval_block_statement(&if_statement.consequence)?
                } else {
                    if let Some(alternative) = &if_statement.alternative {
                        self.eval_block_statement(alternative)?;
                    }
                }
            }
            _ => {
                return Err(RuntimeError::Generic(String::from(
                    "Tried to use non-bool expression in if-statement",
                )))
            }
        }
        Ok(())
    }

    fn eval_loop_statement(&mut self, loop_statement: &LoopStatement) -> RuntimeResult<()> {
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
                            _ => {
                                return Err(RuntimeError::Generic(String::from(
                                    "Tried to use a non-bool expression in a break-if statement",
                                )))
                            }
                        }
                    }
                    statement => self.eval_statement(statement)?,
                }
            }
        }
        self.env.exit_scope();
        Ok(())
    }

    fn eval_expression_statement(&mut self, expr: &ExpressionStatement) -> RuntimeResult<()> {
        match expr {
            ExpressionStatement::Call(call_expr) => self.eval_call_expr(call_expr)?,
        };
        Ok(())
    }

    fn eval_expr(&mut self, expr: &Expression) -> RuntimeResult<Object> {
        let result = match expr {
            Expression::Ident(ident_expr) => self.eval_ident_expr(ident_expr),
            Expression::FloatLiteral(lit) => Ok(Object::Float(*lit)),
            Expression::StringLiteral(string) => Ok(Object::String(string.clone())),
            Expression::BoolLiteral(boolean) => Ok(Object::Bool(*boolean)),
            Expression::Call(call_expr) => {
                if let Some(out_value) = self.eval_call_expr(call_expr)? {
                    Ok(out_value)
                } else {
                    Err(RuntimeError::Generic(String::from(
                        "Tried to use call expression with an invalid procedure",
                    )))
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
        };
        result
    }

    fn eval_infix_expr(
        &mut self,
        kind: InfixKind,
        left: &Object,
        right: &Object,
    ) -> RuntimeResult<Object> {
        // TODO: Handle string concatenation with non-string types (implicit conversion)
        let result = match left {
            Object::Float(lv) => match right {
                Object::Float(rv) => self.eval_infix_float_expr(kind, *lv, *rv),
                _ => Err(RuntimeError::Generic(String::from(
                    "Tried to apply infix to different types",
                ))),
            },
            Object::String(lv) => match right {
                Object::String(rv) => self.eval_infix_string_expr(kind, lv, rv),
                _ => Err(RuntimeError::Generic(String::from(
                    "Tried to apply infix to different types",
                ))),
            },
            Object::Bool(lv) => match right {
                Object::Bool(rv) => self.eval_infix_bool_expr(kind, *lv, *rv),
                _ => Err(RuntimeError::Generic(String::from(
                    "Tried to apply infix to different types",
                ))),
            },
            _ => Err(RuntimeError::Generic(String::from(
                "Tried to apply infix to different types",
            ))),
        };
        result
    }

    fn eval_infix_float_expr(
        &mut self,
        kind: InfixKind,
        left: f64,
        right: f64,
    ) -> RuntimeResult<Object> {
        let result = match kind {
            InfixKind::Plus => Ok(Object::Float(left + right)),
            InfixKind::Minus => Ok(Object::Float(left - right)),
            InfixKind::Multiply => Ok(Object::Float(left * right)),
            InfixKind::Divide => Ok(Object::Float(left / right)),
            InfixKind::Power => Ok(Object::Float(left.powf(right))),
            InfixKind::Equal => Ok(Object::Bool((left - right).abs() <= std::f64::EPSILON)),
            InfixKind::NotEqual => Ok(Object::Bool((left - right).abs() > std::f64::EPSILON)),
            InfixKind::LessThan => Ok(Object::Bool(left < right)),
            InfixKind::LessThanEqual => Ok(Object::Bool(left <= right)),
            InfixKind::GreaterThan => Ok(Object::Bool(left > right)),
            InfixKind::GreaterThanEqual => Ok(Object::Bool(left >= right)),
            _ => Err(RuntimeError::Generic(format!(
                "Tried to apply invalid infix to float: {:?}",
                kind
            ))),
        };
        result
    }

    fn eval_infix_string_expr(
        &mut self,
        kind: InfixKind,
        left: &str,
        right: &str,
    ) -> RuntimeResult<Object> {
        let result = match kind {
            InfixKind::Plus => Ok(Object::String(format!("{}{}", left, right))),
            _ => Err(RuntimeError::Generic(String::from(
                "Tried to apply invalid infix to string",
            ))),
        };
        result
    }

    fn eval_infix_bool_expr(
        &mut self,
        kind: InfixKind,
        lv: bool,
        rv: bool,
    ) -> RuntimeResult<Object> {
        let result = match kind {
            InfixKind::Or => Ok(Object::Bool(lv || rv)),
            InfixKind::And => Ok(Object::Bool(lv && rv)),
            _ => Err(RuntimeError::Generic(String::from(
                "Tried to apply invalid infix to bool",
            ))),
        };
        result
    }

    fn eval_prefix_expr(&mut self, kind: PrefixKind, right: &Object) -> RuntimeResult<Object> {
        let result = match right {
            Object::Bool(v) => match kind {
                PrefixKind::Not => Ok(Object::Bool(!*v)),
                _ => Err(RuntimeError::Generic(String::from(
                    "Tried to apply invalid prefix to bool",
                ))),
            },
            Object::Float(v) => match kind {
                PrefixKind::Plus => Ok(Object::Float(*v)),
                PrefixKind::Minus => Ok(Object::Float(-*v)),
                _ => Err(RuntimeError::Generic(String::from(
                    "Tried to apply invalid prefix to float",
                ))),
            },
            _ => Err(RuntimeError::Generic(String::from(
                "Tried to apply prefix to invalid type",
            ))),
        };
        result
    }

    fn eval_index_expr(&mut self, index_expr: &IndexExpression) -> RuntimeResult<Object> {
        let ident_string = &index_expr.ident.value;
        let indices = &index_expr.indices;

        let indices_string = self.indices_to_string(&indices)?;

        let ident_string = format!("%{}{}", ident_string, indices_string);
        self.env.get(&ident_string)
    }

    fn eval_call_expr(&mut self, call_expr: &CallExpression) -> RuntimeResult<Option<Object>> {
        let proc_ident_str = &call_expr.procedure_ident.value;
        let proc = self.env.get_procedure(&proc_ident_str)?.clone();
        let proc_params = match &proc {
            Procedure::UserDefined(user_proc) => &user_proc.parameters,
            Procedure::Builtin(builtin_proc) => &builtin_proc.parameters,
        }
        .clone();

        let mut call_object_args = self
            .eval_arguments(&call_expr.arguments, &proc_params)?
            .clone();

        let out_values = self.eval_procedure(&proc, &call_object_args)?;

        let call_expr_args = call_expr.arguments.clone();
        let mut params_args = proc_params.iter().zip(call_expr_args.iter());

        let mut i: usize = 0;
        for param_arg in params_args {
            if param_arg.0.is_out {
                match param_arg.1 {
                    Expression::Ident(ident_expr) => {
                        let out_value = out_values[i].clone();
                        self.env.set(ident_expr.value.clone(), out_value)
                    }
                    _ => {
                        return Err(RuntimeError::Generic(String::from(
                            "Tried to set value to an «out» parameter that is not an identifier",
                        )))
                    }
                }
                i += 1;
            }
        }
        if out_values.len() == 1 {
            Ok(Some(out_values[0].clone()))
        } else {
            Ok(None)
        }
    }

    fn eval_procedure(&mut self, proc: &Procedure, args: &[Object]) -> RuntimeResult<Vec<Object>> {
        self.env.enter_call_stack();
        let mut out_values: Vec<Object> = vec![];
        match proc {
            Procedure::UserDefined(user_proc) => {
                // bind the arguments to parameters' name
                let mut params_args = user_proc.parameters.iter().zip(args.iter());
                for param_arg in params_args {
                    let param_ident_string = param_arg.0.ident.value.clone();
                    let arg_value = param_arg.1.clone();
                    self.env.set(param_ident_string, arg_value);
                }

                // execute the procedure
                for statement in &user_proc.block {
                    self.eval_statement(statement)?;
                }

                // return the values of parameters with the «out» attribute
                for param in &user_proc.parameters {
                    if param.is_out {
                        out_values.push(self.env.get(&param.ident.value)?);
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

    fn eval_ident_expr(&mut self, ident_expr: &IdentExpression) -> RuntimeResult<Object> {
        self.env.get(&ident_expr.value)
    }

    fn eval_arguments(
        &mut self,
        args: &[Expression],
        params: &[Parameter],
    ) -> RuntimeResult<Vec<Object>> {
        let mut obj_args: Vec<Object> = vec![];

        let mut args_params = args.iter().zip(params);

        for arg_param in args_params {
            if arg_param.1.is_in {
                let arg = self.eval_expr(arg_param.0)?;
                obj_args.push(arg);
            } else if arg_param.1.is_out {
                match arg_param.0 {
                    Expression::Ident(ident_expr) => {
                        self.env.set(ident_expr.value.clone(), Object::None);
                        obj_args.push(Object::None);
                    }
                    _ => {
                        return Err(RuntimeError::Generic(String::from(
                            "Expected identifier in «out» argument",
                        )))
                    }
                }
            }
        }
        Ok(obj_args)
    }

    fn indices_to_string(&mut self, indices: &[Expression]) -> RuntimeResult<String> {
        let mut index_str = String::new();
        for index_expr in indices {
            let index_obj = self.eval_expr(&index_expr)?; // get index object
            let index = match index_obj {
                // convert index object to integer
                Object::Float(index_f64) => index_f64 as u64,
                _ => {
                    return Err(RuntimeError::Generic(String::from(
                        "Tried to convert non-integer index to string",
                    )))
                }
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
        Evaluator::new(Environment::new_builtin())
            .eval(
                Parser::new(Lexer::new(
                    r#"
                    procedure main() {
                        input("fibonacci n: ", n);
                        fibonacci(n, result);
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
                .expect("panicked at parser"),
            )
            .expect("panicked at evaluator");
    }
}
