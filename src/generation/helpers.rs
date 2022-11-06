use lightningcss::rules::media::MediaRule;
use lightningcss::rules::CssRule;

#[derive(Clone)]
pub enum Node<'a> {
    Media(&'a MediaRule<'a>),
}

impl<'a> From<&'a CssRule<'a>> for Node<'a> {
    fn from(rule: &'a CssRule<'a>) -> Node<'a> {
        match rule {
            CssRule::Media(media) => Node::Media(media),
            CssRule::Import(_) => todo!(),
            CssRule::Style(_) => todo!(),
            CssRule::Keyframes(_) => todo!(),
            CssRule::FontFace(_) => todo!(),
            CssRule::Page(_) => todo!(),
            CssRule::Supports(_) => todo!(),
            CssRule::CounterStyle(_) => todo!(),
            CssRule::Namespace(_) => todo!(),
            CssRule::MozDocument(_) => todo!(),
            CssRule::Nesting(_) => todo!(),
            CssRule::Viewport(_) => todo!(),
            CssRule::CustomMedia(_) => todo!(),
            CssRule::Ignored => todo!(),
            CssRule::FontPaletteValues(_) => todo!(),
            CssRule::LayerStatement(_) => todo!(),
            CssRule::LayerBlock(_) => todo!(),
            CssRule::Property(_) => todo!(),
            CssRule::Container(_) => todo!(),
            CssRule::Unknown(_) => todo!(),
        }
    }
}
