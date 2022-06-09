/// Represents a tree of native_mock components.
#[derive(PartialEq, Debug)]
pub(super) struct Repr {
    pub name: String,
    pub value: String,
    pub has_on_click: bool,
    pub children: Vec<Repr>,
}
