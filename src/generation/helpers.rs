#[derive(Clone)]
pub enum Node<'a> {
    Declaration(raffia::ast::Declaration<'a>),
    QualifiedRule(raffia::ast::QualifiedRule<'a>),
    AtRule(raffia::ast::AtRule<'a>),
    KeyframeBlock(raffia::ast::KeyframeBlock<'a>),
}

impl<'a> From<raffia::ast::Statement<'a>> for Node<'a> {
    fn from(rule: raffia::ast::Statement<'a>) -> Node<'a> {
        match rule {
            raffia::ast::Statement::AtRule(at_rule) => Node::AtRule(at_rule),
            raffia::ast::Statement::Declaration(declaration) => Node::Declaration(declaration),
            raffia::ast::Statement::KeyframeBlock(keyframe_block) => {
                Node::KeyframeBlock(keyframe_block)
            }
            raffia::ast::Statement::LessVariableDeclaration(_) => todo!(),
            raffia::ast::Statement::QualifiedRule(rule) => Node::QualifiedRule(rule),
            raffia::ast::Statement::SassContentAtRule(_) => todo!(),
            raffia::ast::Statement::SassDebugAtRule(_) => todo!(),
            raffia::ast::Statement::SassEachAtRule(_) => todo!(),
            raffia::ast::Statement::SassErrorAtRule(_) => todo!(),
            raffia::ast::Statement::SassExtendAtRule(_) => todo!(),
            raffia::ast::Statement::SassForAtRule(_) => todo!(),
            raffia::ast::Statement::SassForwardAtRule(_) => todo!(),
            raffia::ast::Statement::SassFunctionAtRule(_) => todo!(),
            raffia::ast::Statement::SassIfAtRule(_) => todo!(),
            raffia::ast::Statement::SassIncludeAtRule(_) => todo!(),
            raffia::ast::Statement::SassMixinAtRule(_) => todo!(),
            raffia::ast::Statement::SassReturnAtRule(_) => todo!(),
            raffia::ast::Statement::SassUseAtRule(_) => todo!(),
            raffia::ast::Statement::SassVariableDeclaration(_) => todo!(),
            raffia::ast::Statement::SassWarnAtRule(_) => todo!(),
            raffia::ast::Statement::SassWhileAtRule(_) => todo!(),
        }
    }
}
