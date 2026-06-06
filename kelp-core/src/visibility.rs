#[derive(Debug, Default, Clone, Copy, Hash)]
pub enum Visibility {
    Public,
    #[default]
    None,
}
