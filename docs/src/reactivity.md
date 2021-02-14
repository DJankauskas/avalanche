# Reactivity

The previous chapter showed how even with state, we can write our UI declaratively. But how, exactly, does this work?
In theory, `avalanche` could rerender our entire app on every state change, but in practice, that's often too inefficient.
Instead, `avalanche` uses reactivity. The `#[component]` macro tracks what parameters and hook values influence the value of each 
parameter of children elements. Then, the framework updates only child parameters whose input parameters or hook values changed (although everything in 
a component function is run on rerender).

Usually, you won't need to worry about reactivity, but occasionally, it's helpful to know the details. As an example, let's 
say we want to show the user a random number, and update it on a button click. One potential approach could be to trigger a rerender
by calling a `UseState` setter.

```rust
# use avalanche::{component, View, UseState};
# use avalanche_web::components::{Text, Div, Button};
use rand::random;

#[component(trigger_update = UseState<()>)]
fn Random() -> View {
    let (_, trigger_update) = trigger_update(());

    Div! {
        children: [
            Button! {
                child: Text! {text: "Generate new number"},
                on_click: move |_| trigger_update.set(())
            },
            Div! {
                child: Text! {text: random::<u16>()}
            } 
        ]
    }
}
```
However, if you try out the example for yourself, you'll notice that the number doesn't actually update. A reasonable explanation for this would be
we're not actually modifying `trigger_update`'s state, or that its current value is equal to the previous one. However, calling a `UseState` setter _always_ 
triggers a component rerender. Rather, the reason is that `random::<u16>()` does not depend on any state or parameters, so `avalanche` classifies the value 
as constant. To fix that, let's make the value transparently depend on state:

```rust
# use avalanche::{component, View, UseState};
# use avalanche_web::components::{Text, Div, Button};
use rand::random;

#[component(value = UseState<u16>)]
fn Random() -> View {
    let (value, set_value) = value(random::<u16>());

    Div! {
        children: [
            Button! {
                child: Text! {text: "Generate new number"},
                on_click: move |_| set_value.set(random::<u16>())
            },
            Div! {
                child: Text! {text: value}
            } 
        ]
    }
}
```

Now, `text` clearly depends on the `value` state, so when we call `set_value`, `avalanche` will update that text value on screen.

This example leads us to the two main reactivity rules:

### Wrap interior mutability and hidden side effects

For parameter values, directly using values like `rand::random()` is a problem because they depend on some external state
that `avalanche` is not aware of, leading to stale values displayed on rerender. This also applies to values like `Rc<RefCell<T>>`
if they are updated outside of hook functionality. When using interior mutability, then, make sure to wrap it in `UseState`, and perform updates 
within the `update` method.

### Avoid third-party macros

Macros can lead to convenient code; for example, `Text! {text: format!("Hello, {} {}!", rank, name)}` is a lot clearer than the macro-free
alternative. However, when using non-`std` and `avalanche` macros, `#[component]` is unable to track their dependencies correctly, meaning
parameters based on those macros may not update correctly. In the future, we may instead opt to consider those macros always updated (at the cost of 
efficiency). Either way, we recommend you avoid those macros. 

## Performance

Note that whenever a `UseState` value is considered updated, **all** of its subfields and method calls
are considered updated. Thus, instead of using one large state `struct`, consider breaking it apart into its constituent fields,
unless if the `struct`'s fields are usually updated all at once.

(More performance optimizations will become available in future library updates).