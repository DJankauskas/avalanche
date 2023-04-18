use wasm_bindgen::prelude::{wasm_bindgen, JsValue};

#[wasm_bindgen(module = "/js/bridge.js")]
extern "C" {
    pub(crate) fn append_child(parent: &JsValue, child: &JsValue);
    pub(crate) fn insert_child(parent: &JsValue, idx: u32, child: &JsValue);
    pub(crate) fn swap_children(parent: &JsValue, lesser_idx: u32, greater_idx: u32);
    pub(crate) fn truncate_children(parent: &JsValue, len: u32);
    
    pub(crate) fn intern_string_at(string: &[u16], idx: u32);
    
    pub(crate) fn create_text_node(value_idx: u32) -> JsValue;
    pub(crate) fn create_element(tag_idx: u32) -> JsValue;
    
    pub(crate) fn set_text_content(text_node: &JsValue, value_idx: u32);
    pub(crate) fn set_attribute(element: &JsValue, name_idx: u32, value_idx: u32);
}