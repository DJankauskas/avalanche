# Advanced component props

## Documenting parameters
Documentation is important to ensure components are easy to use and understand.
Parameters can be documented with standard Rust documentation comments and 
attributes:

```rust
# use avalanche::{component, tracked, View};
# use avalanche_web::components::Text;

/// Repeats a character for the specified number of times.
#[component]
fn Repeat(
    /// The character to repeat.
    character: char, 
    #[doc = "How many times the character is repeated."]
    n: usize,
) -> View {
    Text(self, tracked!(character).to_string().repeat(tracked!(n)))
}
```

## Default parameters

Imagine that we wanted to hide the `Repeat` component if the user does not 
supply a value for `n`. We can do this by using `usize`'s default value:


```rust
# use avalanche::{component, tracked, View};
# use avalanche_web::components::Text;

#[component]
fn Repeat(
    character: char, 
    // How many times the character is repeated.
    #[default]
    n: usize
) -> View {
    Text(self, tracked!(character).to_string().repeat(tracked!(n)))
}
```

Specifying custom default values is also possible. If we want
to make `Repeat` excitement-themed, we could choose a default character value `'!'`:

```rust
# use avalanche::{component, tracked, View};
# use avalanche_web::components::Text;

#[component]
fn Repeat(
    #[default = '!']
    character: char, 
    #[default]
    // How many times the character is repeated.
    n: usize
) -> View {
    Text(self, tracked!(character).to_string().repeat(tracked!(n)))
}
```

The component can now be called with `Repeat(self)`, which would render nothing,
or `Repeat(self, count = 3)`, which would render `!!!`.

A default value can be any valid Rust expression. The expression will only 
be evaluated if the caller of the component does not supply their own value.

## Generic components

Generic components can be written similarly to generic functions:

```rust
# use avalanche::{component, tracked, View};
# use avalanche_web::components::Text;
use std::string::ToString;

#[component]
fn SayHello<S: ToString>(name: S) -> View {
    Text(self, format!("Hello, {}!", tracked!(name).to_string()))
}
```

> Note that `impl Trait` properties are not supported in components.
