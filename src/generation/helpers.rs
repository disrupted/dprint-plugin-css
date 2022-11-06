use raffia::ast::Statement;

#[derive(Clone)]
pub enum Node<'a> {
    Media(Statement<'a>),
}

impl<'a> From<Statement<'a>> for Node<'a> {
    fn from(rule: Statement<'a>) -> Node<'a> {
        match rule {
            Statement::AtRule(_) => todo!(),
            Statement::Declaration(_) => todo!(),
            Statement::KeyframeBlock(_) => todo!(),
            Statement::LessVariableDeclaration(_) => todo!(),
            Statement::QualifiedRule(_) => todo!(),
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
            // Statement::Media(media) => Node::Media(media),
            // Statement::Import(_) => todo!(),
            // Statement::Style(_) => todo!(),
            // Statement::Keyframes(_) => todo!(),
            // Statement::FontFace(_) => todo!(),
            // Statement::Page(_) => todo!(),
            // Statement::Supports(_) => todo!(),
            // Statement::CounterStyle(_) => todo!(),
            // Statement::Namespace(_) => todo!(),
            // Statement::MozDocument(_) => todo!(),
            // Statement::Nesting(_) => todo!(),
            // Statement::Viewport(_) => todo!(),
            // Statement::CustomMedia(_) => todo!(),
            // Statement::Ignored => todo!(),
            // Statement::FontPaletteValues(_) => todo!(),
            // Statement::LayerStatement(_) => todo!(),
            // Statement::LayerBlock(_) => todo!(),
            // Statement::Property(_) => todo!(),
            // Statement::Container(_) => todo!(),
            // Statement::Unknown(_) => todo!(),
        }
    }
}
