use raffia::ast::{AtRule, Declaration, QualifiedRule, Statement};

#[derive(Clone)]
pub enum Node<'a> {
    Declaration(Declaration<'a>),
    QualifiedRule(QualifiedRule<'a>),
    AtRule(AtRule<'a>),
}

impl<'a> From<Statement<'a>> for Node<'a> {
    fn from(rule: Statement<'a>) -> Node<'a> {
        match rule {
            Statement::AtRule(at_rule) => Node::AtRule(at_rule),
            Statement::Declaration(declaration) => Node::Declaration(declaration),
            Statement::KeyframeBlock(_) => todo!(),
            Statement::LessVariableDeclaration(_) => todo!(),
            Statement::QualifiedRule(rule) => Node::QualifiedRule(rule),
            Statement::SassContentAtRule(_) => todo!(),
            Statement::SassDebugAtRule(_) => todo!(),
            Statement::SassEachAtRule(_) => todo!(),
            Statement::SassErrorAtRule(_) => todo!(),
            Statement::SassExtendAtRule(_) => todo!(),
            Statement::SassForAtRule(_) => todo!(),
            Statement::SassForwardAtRule(_) => todo!(),
            Statement::SassFunctionAtRule(_) => todo!(),
            Statement::SassIfAtRule(_) => todo!(),
            Statement::SassIncludeAtRule(_) => todo!(),
            Statement::SassMixinAtRule(_) => todo!(),
            Statement::SassReturnAtRule(_) => todo!(),
            Statement::SassUseAtRule(_) => todo!(),
            Statement::SassVariableDeclaration(_) => todo!(),
            Statement::SassWarnAtRule(_) => todo!(),
            Statement::SassWhileAtRule(_) => todo!(),
        }
    }
}
