use dprint_core::configuration::NewLineKind;
use serde::{Deserialize, Serialize};

#[derive(Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Configuration {
    pub use_tabs: bool,
    pub indent_width: u8,
    pub line_width: u32,
    pub new_line_kind: NewLineKind,
}
