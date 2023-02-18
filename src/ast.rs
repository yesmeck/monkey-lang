use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let(s) => write!(f, "{}", s),
            Self::Return(s) => write!(f, "{}", s),
            Self::Expression(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: Expression,
}

impl LetStatement {
    pub fn new(name: Identifier, value: Expression) -> Self {
        Self { name, value }
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {}", self.name, self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    pub return_value: Expression,
}

impl ReturnStatement {
    pub fn new(return_value: Expression) -> Self {
        Self { return_value }
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {}", self.return_value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

impl ExpressionStatement {
    pub fn new(expression: Expression) -> Self {
        Self { expression }
    }
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(|s| format!("{}", s))
                .collect::<Vec<String>>()
                .join::<&str>("")
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(Identifier),
    NullLiteral(NullLiteral),
    IntegerLiteral(IntegerLiteral),
    StringLiteral(StringLiteral),
    ArrayLiteral(ArrayLiteral),
    HashLiteral(HashLiteral),
    FunctionLiteral(FunctionLiteral),
    Boolean(BooleanExpression),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Call(CallExpression),
    If(IfExpression),
    Index(IndexExpression),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(e) => write!(f, "{}", e),
            Self::NullLiteral(e) => write!(f, "{}", e),
            Self::IntegerLiteral(e) => write!(f, "{}", e),
            Self::StringLiteral(e) => write!(f, "{}", e),
            Self::ArrayLiteral(e) => write!(f, "{}", e),
            Self::HashLiteral(e) => write!(f, "{}", e),
            Self::FunctionLiteral(e) => write!(f, "{}", e),
            Self::Boolean(e) => write!(f, "{}", e),
            Self::Prefix(e) => write!(f, "{}", e),
            Self::Infix(e) => write!(f, "{}", e),
            Self::Index(e) => write!(f, "{}", e),
            Self::Call(e) => write!(f, "{}", e),
            Self::If(e) => write!(f, "{}", e),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub value: String,
}

impl Identifier {
    pub fn new(value: String) -> Self {
        Self { value }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct NullLiteral {}

impl Display for NullLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "null")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntegerLiteral {
    pub value: i64,
}

impl IntegerLiteral {
    pub fn new(value: i64) -> Self {
        Self { value }
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringLiteral {
    pub value: String,
}

impl StringLiteral {
    pub fn new(value: String) -> Self {
        Self { value }
    }
}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
}

impl ArrayLiteral {
    pub fn new(elements: Vec<Expression>) -> Self {
        Self { elements }
    }
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.elements
                .iter()
                .map(|e| format!("{e}"))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct HashLiteral {
    pub members: Vec<HashMember>,
}

impl HashLiteral {
    pub fn new(members: Vec<HashMember>) -> Self {
        Self { members }
    }
}

impl Display for HashLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.members
                .iter()
                .map(|m| format!("{m}"))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct HashMember {
    pub key: Expression,
    pub value: Expression,
}

impl HashMember {
    pub fn new(key: Expression, value: Expression) -> Self {
        Self { key, value }
    }
}

impl Display for HashMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.key, self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionLiteral {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn({}) {}",
            self.parameters
                .iter()
                .map(|p| format!("{}", p))
                .collect::<Vec<String>>()
                .join(", "),
            self.body
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    pub operator: String,
    pub right: Box<Expression>,
}

impl PrefixExpression {
    pub fn new(operator: String, right: Expression) -> Self {
        Self {
            operator,
            right: Box::new(right),
        }
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    pub operator: String,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl InfixExpression {
    pub fn new(operator: String, left: Expression, right: Expression) -> Self {
        Self {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BooleanExpression {
    pub value: bool,
}

impl BooleanExpression {
    pub fn new(value: bool) -> Self {
        Self { value }
    }
}

impl Display for BooleanExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref alternative) = self.alternative {
            write!(
                f,
                "if {} {} else {}",
                self.condition, self.consequence, alternative
            )
        } else {
            write!(f, "if {} {}", self.condition, self.consequence)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl CallExpression {
    pub fn new(function: Expression, arguments: Vec<Expression>) -> Self {
        Self {
            callee: Box::new(function),
            arguments,
        }
    }
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            self.callee,
            self.arguments
                .iter()
                .map(|a| format!("{}", a))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IndexExpression {
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl IndexExpression {
    pub fn new(left: Expression, index: Expression) -> Self {
        Self {
            left: Box::new(left),
            index: Box::new(index),
        }
    }
}

impl Display for IndexExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(|s| format!("{}", s))
                .collect::<Vec<String>>()
                .join::<&str>("")
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_print_identifier() {
        assert_eq!(
            format!(
                "{}",
                Identifier {
                    value: "foo".into()
                }
            ),
            "foo"
        );
    }

    #[test]
    fn test_print_program() {
        let program = Program {
            statements: vec![Statement::Expression(ExpressionStatement::new(
                Expression::Infix(InfixExpression {
                    operator: "+".into(),
                    left: Box::new(Expression::IntegerLiteral(IntegerLiteral::new(1))),
                    right: Box::new(Expression::IntegerLiteral(IntegerLiteral::new(1))),
                }),
            ))],
        };

        assert_eq!(format!("{}", program), "(1 + 1)");
    }
}
