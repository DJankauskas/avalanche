# Hooks, State, and Control Flow

## Hooks

So far, all the data we've passed to parameters and used in general was static, specified at compilation time.
But in real apps, we usually need to maintain state for things like network request statuses and inputs.

Let's say we're trying to write a counter. We'll start out with a stateless counter:

```rust
use avalanche::{component, View};
use avalanche_web::components::{Div, H2, Button, Text};

#[component]
fn Counter() -> View {
    Div! {
        children: [
            H2! {
                child: Text!{text: "Counter!"},
            },
            Button! {
                on_click: |_| todo!(),
                child: Text!{text: "+"}
            },
            Text! {text: 0}
        ]
    }
}
```

Since we don't keep track of the count yet, we've substituted it with the value `0`. `Text`'s `text` parameter accepts any value that implements `ToString`,
so we can pass `0` instead of `"0"`. We've also defined the `on_click` handler for `Button`, so the given closure's code will run on every click. 
Since right now we have no state to update yet, we call `todo!()`, so clicking the button would crash our app. 

Now that we have our starting point, let's finally inject some state. The `UseState` hook allows us to inject
state so we can keep track of the number of clicks. Since we have a count that monotonically increases, we can use an unsigned integer `u64` with `UseState<u64>`.

Let's finally make or counter stateful:

```rust
# use avalanche::{component, View};
# use avalanche_web::components::{Div, H2, Button, Text};

#[component(count = UseState<u64>)]
fn Counter() -> View {
    let (count, set_count) = count(0);

    Div! {
        children: [
            H2! {
                child: Text! {text: "Counter!"},
            },
            Button! {
                on_click: move |_| set_counter.update(|count| *count += 1),
                child: Text! {text: "+"}
            },
            Text! {text: count}
        ]
    }
}
```

> Note that for examples past this point, we will be omitting `avalanche` and `avalanche_web` imports for brevity.

First, notice that we declare hooks within the `#[component]` attribute, with the syntax `#[component(hook_name = HookType, ...)]`. For each 
`hook_name`, this introduces a variable of that name into the component function's scope. Note that each hook name must be unique within a component.

Here, the `count` injected by `UseState` is a function which takes the state's default value for `u64` and returns a tuple with a reference to the 
current state (which we name `count`) and a setter, which we name `set_count`. The setter has an `update` method which accepts a closure that receives a `&mut u64` and modifies it. Every time the user clicks on the button, `on_click` fires, calling `set_counter.update()`, which runs 
`count += 1`, updating the state and causing the component to be rerendered. With that, we have our first stateful component! If we instead wanted to simply 
set the state to a value like `0`, we could write `set_count.set(0)` as a shorthand for `set_count.update(|count| *count = 0)`.

## Dynamic rendering

Oftentimes, we don't just want to update property values based on changes to state and props, but also what children we render.
In some frameworks, that requires special template syntax, but we can take advantage of the fact that the `Component! {}` syntax returns plain 
Rust values to use normal control flow instead.

### Conditional rendering

Want to render something only if a condition is true? Use an `if` statement:

```rust
# use avalanche::{component, View};
# use avalanche_web::components::Text;
#
#[component]
fn Conditional(cond: bool) -> View {
    if *cond {
        Text! {text: "True!"}
    } else {
        ().into()
    }
}
```

The `()` type is a `Component` that renders nothing. We call `.into()` on it to turn it into a `View`.

There is also a shorthand for this some component-or-nothing case:

```rust
# use avalanche::{component, View};
# use avalanche_web::components::Text;
#
#[component]
fn Conditional(cond: bool) -> View {
    *cond.then(|| Text! {text: "True!"}).into()
}
```

That's equivalent to the above example. Of course, if you want more complex conditionals, you can use other control flow like `else if` and `match`.

> The `then` bool method is available in Rust versions 1.50+.

### Lists

We can use similar techniques to render lists of content:

```rust
# use avalanche::{component, View};
# use avalanche_web::components::{Ul, Li, Text};
#
#[component]
fn List(items: Vec<String>) -> View {
    Ul! {
        children: items.iter().map(|item| Li! {
            child: Text! {text: item},
            key: item
        }).collect()
    }
}
```

Here, we use the standard iterator methods `map` and `collect` to turn a `Vec` of strings into a `Vec` of `View`s.
This is mostly self-explanatory, but the important thing to note is the `key` parameter on `Li`. Whenever an instance of `Component!` in code 
may be called more than once, it is required to specify a key to disambiguate the multiple instantiations; this is enforced with a runtime error if not specified. Note that the key must be added on the topmost level, so in our example above,
```
Li! {
    child: Text! {text: item},
    key: item
}
```
is good but this is not:
```
Li! {
    child: Text! {
        text: item,
        key: item
    }
}
```