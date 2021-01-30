# Roadmap
## avalanche-macro
- ~~supply input function properties, like `pub`, to resulting structures~~
- fine-grained dependency tracking
- allow writing custom hooks with `#[hook]`
- generate Builder structs that statically ensure mandatory properties are supplied, as in [typed-builder](https://github.com/idanarye/rust-typed-builder)
    - blocked until `min_const_generics` stabilizes
## avalanche
- ~~allow hooks (currently only `UseState`) to provide update info~~
- ~~introduce keys to increase children performance, and automatically add them for static child layout~~
- support batch state updates more performantly
- reduce memory stored and copied in components
## avalanche-web
- ~~implement all base HTML tags and their associated properties~~
- add all callbacks, as well as access to things like capture phase listeners
- server-side rendering
