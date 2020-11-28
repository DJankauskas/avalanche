# Avalanche
Avalanche is a performant library for building Web applications. It analyzes the relationships between properties and state in functional component declarations, 
generating code that efficiently creates and updates the application UI. At the moment, however, updates may be applied too conservatively or aggressively, 
and most web platform features are as yet unsupported.

## How it works
Functions marked with `#[component]` are analyzed to determine what properties and instances of state might influence a child's property. If any of those have changed, that
property will be marked as updated, while unmarked properties will be ignored (except on first render).

## Roadmap
#### avalanche-macro
- ~~supply input function properties, like `pub`, to resulting structures~~
- allow writing custom hooks with `#[hook]`
- generate Builder structs that statically ensure mandatory properties are supplied, as in [typed-builder](https://github.com/idanarye/rust-typed-builder)
    - blocked until `min_const_generics` stabilizes
#### avalanche
- ~~allow hooks (currently only `UseState`) to provide update info~~
- ~~introduce keys to increase children performance, and automatically add them for static child layout~~
- support batch state updates more performantly
- reduce memory stored and copied in components
#### avalanche-web
- implement all base HTML tags and their associated properties
- provide access to callback parameters and options like `passive`
- server-side rendering
