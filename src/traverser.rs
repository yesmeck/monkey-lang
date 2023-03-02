use crate::ast::{
    ArrayLiteral, BlockStatement, BooleanExpression, CallExpression, Expression,
    ExpressionStatement, FunctionLiteral, HashLiteral, Identifier, IfExpression, IndexExpression,
    InfixExpression, IntegerLiteral, LetStatement, MacroLiteral, NullLiteral, PrefixExpression,
    Program, ReturnStatement, Statement, StringLiteral,
};

pub trait Visitor {
    fn visit_mut_program(&self, _node: &mut Program) {}
    fn visit_mut_expression_statement(&self, _node: &mut ExpressionStatement) {}
    fn visit_mut_let_statement(&self, _node: &mut LetStatement) {}
    fn visit_mut_return_statement(&self, _node: &mut ReturnStatement) {}
    fn visit_mut_null_literal(&self, _node: &mut NullLiteral) {}
    fn visit_mut_integer_literal(&self, _node: &mut IntegerLiteral) {}
    fn visit_mut_string_literal(&self, _node: &mut StringLiteral) {}
    fn visit_mut_array_literal(&self, _node: &mut ArrayLiteral) {}
    fn visit_mut_hash_literal(&self, _node: &mut HashLiteral) {}
    fn visit_mut_function_literal(&self, _node: &mut FunctionLiteral) {}
    fn visit_mut_macro_literal(&self, _node: &mut MacroLiteral) {}
    fn visit_mut_identifer(&self, _node: &mut Identifier) {}
    fn visit_mut_expression(&self, _node: &mut Expression) {}
    fn visit_mut_boolean_expression(&self, _node: &mut BooleanExpression) {}
    fn visit_mut_if_expression(&self, _node: &mut IfExpression) {}
    fn visit_mut_prefix_expression(&self, _node: &mut PrefixExpression) {}
    fn visit_mut_infix_expression(&self, _node: &mut InfixExpression) {}
    fn visit_mut_index_expression(&self, _node: &mut IndexExpression) {}
    fn visit_mut_call_expression(&self, _node: &mut CallExpression) {}
}

pub trait Traverable {
    fn visit_mut(&mut self, _visitor: &impl Visitor) {}
}

impl Traverable for Program {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        visitor.visit_mut_program(self);

        for stmt in self.statements.iter_mut() {
            stmt.visit_mut(visitor);
        }
    }
}

impl Traverable for Statement {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        match self {
            Statement::Let(n) => n.visit_mut(visitor),
            Statement::Return(n) => n.visit_mut(visitor),
            Statement::Expression(n) => n.visit_mut(visitor),
        }
    }
}

impl Traverable for ExpressionStatement {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        visitor.visit_mut_expression_statement(self);
        self.expression.visit_mut(visitor);
    }
}

impl Traverable for Expression {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        visitor.visit_mut_expression(self);
        match self {
            Expression::NullLiteral(n) => n.visit_mut(visitor),
            Expression::IntegerLiteral(n) => n.visit_mut(visitor),
            Expression::StringLiteral(n) => n.visit_mut(visitor),
            Expression::ArrayLiteral(n) => n.visit_mut(visitor),
            Expression::HashLiteral(n) => n.visit_mut(visitor),
            Expression::FunctionLiteral(n) => n.visit_mut(visitor),
            Expression::MacroLiteral(n) => n.visit_mut(visitor),
            Expression::Identifier(n) => n.visit_mut(visitor),
            Expression::Boolean(n) => n.visit_mut(visitor),
            Expression::Prefix(n) => n.visit_mut(visitor),
            Expression::Infix(n) => n.visit_mut(visitor),
            Expression::Index(n) => n.visit_mut(visitor),
            Expression::Call(n) => n.visit_mut(visitor),
            Expression::If(n) => n.visit_mut(visitor),
        }
    }
}

impl Traverable for BooleanExpression {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        visitor.visit_mut_boolean_expression(self);
    }
}

impl Traverable for NullLiteral {
    fn visit_mut(&mut self, _visitor: &impl Visitor) {
        _visitor.visit_mut_null_literal(self)
    }
}

impl Traverable for IntegerLiteral {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        visitor.visit_mut_integer_literal(self);
    }
}

impl Traverable for StringLiteral {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        visitor.visit_mut_string_literal(self);
    }
}

impl Traverable for ArrayLiteral {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        visitor.visit_mut_array_literal(self);
        for elm in self.elements.iter_mut() {
            elm.visit_mut(visitor);
        }
    }
}

impl Traverable for HashLiteral {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        visitor.visit_mut_hash_literal(self);
        for member in self.members.iter_mut() {
            member.key.visit_mut(visitor);
            member.value.visit_mut(visitor);
        }
    }
}

impl Traverable for FunctionLiteral {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        visitor.visit_mut_function_literal(self);
        for parameter in self.parameters.iter_mut() {
            parameter.visit_mut(visitor);
        }
        self.body.visit_mut(visitor);
    }
}

impl Traverable for MacroLiteral {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        visitor.visit_mut_macro_literal(self);
        for parameter in self.parameters.iter_mut() {
            parameter.visit_mut(visitor);
        }
        self.body.visit_mut(visitor);
    }
}

impl Traverable for Identifier {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        visitor.visit_mut_identifer(self);
    }
}

impl Traverable for IfExpression {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        visitor.visit_mut_if_expression(self);
        self.condition.visit_mut(visitor);
        self.consequence.visit_mut(visitor);
        if let Some(mut alternative) = self.alternative.take() {
            alternative.visit_mut(visitor);
            self.alternative = Some(alternative);
        }
    }
}

impl Traverable for LetStatement {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        visitor.visit_mut_let_statement(self);
        self.name.visit_mut(visitor);
        self.value.visit_mut(visitor);
    }
}

impl Traverable for ReturnStatement {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        visitor.visit_mut_return_statement(self);
        self.return_value.visit_mut(visitor);
    }
}

impl Traverable for BlockStatement {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        for stmt in self.statements.iter_mut() {
            stmt.visit_mut(visitor);
        }
    }
}

impl Traverable for PrefixExpression {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        visitor.visit_mut_prefix_expression(self);
        self.right.visit_mut(visitor);
    }
}

impl Traverable for InfixExpression {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        visitor.visit_mut_infix_expression(self);
        self.left.visit_mut(visitor);
        self.right.visit_mut(visitor);
    }
}

impl Traverable for CallExpression {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        visitor.visit_mut_call_expression(self);
        self.callee.visit_mut(visitor);
        for arg in self.arguments.iter_mut() {
            arg.visit_mut(visitor);
        }
    }
}

impl Traverable for IndexExpression {
    fn visit_mut(&mut self, visitor: &impl Visitor) {
        visitor.visit_mut_index_expression(self);
        self.left.visit_mut(visitor);
        self.index.visit_mut(visitor);
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{
            ArrayLiteral, BlockStatement, Expression, ExpressionStatement, FunctionLiteral,
            HashLiteral, HashMember, Identifier, IfExpression, IndexExpression, InfixExpression,
            IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement, Statement,
        },
        traverser::{Traverable, Visitor},
    };

    #[test]
    fn test_traverse() {
        #[derive(Default)]
        struct TurnOnIntoTwo;

        fn one() -> Expression {
            Expression::IntegerLiteral(IntegerLiteral::new(1))
        }

        fn two() -> Expression {
            Expression::IntegerLiteral(IntegerLiteral::new(2))
        }

        impl Visitor for TurnOnIntoTwo {
            fn visit_mut_integer_literal(&self, node: &mut IntegerLiteral) {
                if node.value == 1 {
                    node.value = 2;
                }
            }
        }

        let tests = vec![
            (
                Statement::Expression(ExpressionStatement::new(one())),
                Statement::Expression(ExpressionStatement::new(two())),
            ),
            (
                Statement::Expression(ExpressionStatement::new(Expression::Infix(
                    InfixExpression::new("+".into(), one(), two()),
                ))),
                Statement::Expression(ExpressionStatement::new(Expression::Infix(
                    InfixExpression::new("+".into(), two(), two()),
                ))),
            ),
            (
                Statement::Expression(ExpressionStatement::new(Expression::Prefix(
                    PrefixExpression::new("-".into(), one()),
                ))),
                Statement::Expression(ExpressionStatement::new(Expression::Prefix(
                    PrefixExpression::new("-".into(), two()),
                ))),
            ),
            (
                Statement::Expression(ExpressionStatement::new(Expression::Index(
                    IndexExpression::new(one(), two()),
                ))),
                Statement::Expression(ExpressionStatement::new(Expression::Index(
                    IndexExpression::new(two(), two()),
                ))),
            ),
            (
                Statement::Expression(ExpressionStatement::new(Expression::If(IfExpression {
                    condition: Box::new(one()),
                    consequence: BlockStatement {
                        statements: vec![Statement::Expression(ExpressionStatement::new(one()))],
                    },
                    alternative: Some(BlockStatement {
                        statements: vec![Statement::Expression(ExpressionStatement::new(one()))],
                    }),
                }))),
                Statement::Expression(ExpressionStatement::new(Expression::If(IfExpression {
                    condition: Box::new(two()),
                    consequence: BlockStatement {
                        statements: vec![Statement::Expression(ExpressionStatement::new(two()))],
                    },
                    alternative: Some(BlockStatement {
                        statements: vec![Statement::Expression(ExpressionStatement::new(two()))],
                    }),
                }))),
            ),
            (
                Statement::Return(ReturnStatement::new(one())),
                Statement::Return(ReturnStatement::new(two())),
            ),
            (
                Statement::Let(LetStatement::new(Identifier::new("one".into()), one())),
                Statement::Let(LetStatement::new(Identifier::new("one".into()), two())),
            ),
            (
                Statement::Expression(ExpressionStatement::new(Expression::FunctionLiteral(
                    FunctionLiteral::new(
                        "foo".into(),
                        vec![],
                        BlockStatement {
                            statements: vec![Statement::Expression(
                                ExpressionStatement::new(one()),
                            )],
                        },
                    ),
                ))),
                Statement::Expression(ExpressionStatement::new(Expression::FunctionLiteral(
                    FunctionLiteral::new(
                        "foo".into(),
                        vec![],
                        BlockStatement {
                            statements: vec![Statement::Expression(
                                ExpressionStatement::new(two()),
                            )],
                        },
                    ),
                ))),
            ),
            (
                Statement::Expression(ExpressionStatement::new(Expression::ArrayLiteral(
                    ArrayLiteral::new(vec![one()]),
                ))),
                Statement::Expression(ExpressionStatement::new(Expression::ArrayLiteral(
                    ArrayLiteral::new(vec![two()]),
                ))),
            ),
            (
                Statement::Expression(ExpressionStatement::new(Expression::HashLiteral(
                    HashLiteral::new(vec![HashMember {
                        key: one(),
                        value: one(),
                    }]),
                ))),
                Statement::Expression(ExpressionStatement::new(Expression::HashLiteral(
                    HashLiteral::new(vec![HashMember {
                        key: two(),
                        value: two(),
                    }]),
                ))),
            ),
        ];

        for (input, output) in tests.iter() {
            let mut program = Program {
                statements: vec![input.to_owned()],
            };

            let expect = Program {
                statements: vec![output.to_owned()],
            };

            let visitor = TurnOnIntoTwo::default();

            program.visit_mut(&visitor);

            assert_eq!(program, expect);
        }
    }
}
