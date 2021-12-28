# Hooks, State, and Control Flow

Earlier, we described components as pure functions. Hooks allow us 
to introduce external data, state, and other impure data into our components.

## Hooks

So far, all the data we've passed to parameters and used in general was static, specified at compilation time.
But in real apps, we usually need to maintain state for things like network request statuses and inputs.

Let's say we're trying to write a counter. We'll start out with a stateless counter:

```rust
use avalanche::{component, tracked, View};
use avalanche_web::components::{Div, H2, Button, Text};

#[component]
fn Counter() -> View {
    Div!([
        H2!([
            Text!("Counter!")
        ]),
        Button!(
            on_click: |_| todo!(),
            [
                Text!("+")
            ]
        ),
        Text!(0)
    ])
}
```

Since we don't keep track of the count yet, we've substituted it with the value `0`. `Text`'s `text` parameter accepts any value that implements `ToString`,
so we can pass `0` instead of `"0"`. We've also defined the `on_click` handler for `Button`, so the given closure's code will run on every click. 
Since right now we have no state to update yet, we call `todo!()`, so clicking the button would crash our app. 

Now that we have our starting point, let's finally inject some state. The `state` hook allows us to inject
state so we can keep track of the number of clicks. Since we have a count that monotonically increases, we can use an unsigned integer `u64` with `state<u64, _>`.

Let's finally make our counter stateful with the `state` hook:

```rust
use avalanche::{component, tracked, state, View};
use avalanche_web::components::{Div, H2, Button, Text};

#[component]
fn Counter() -> View {
    let (count, set_count) = state::<u64, _>(self, || 0);

    Div!([
        H2!([
            Text!("Counter!")
        ]),
        Button!(
            on_click: move |_| set_count.update(|count| *count += 1),
            [
                Text!("+")
            ]
        ),
        Text!(tracked!(count))
    ])
}
```

Hooks are function calls that take `self` as their first parameter. This allows `#[component]` to know the function call is 
a hook, allowing it to pass in some internal state. Hooks have a call site identity, so the above `state` call will return a reference
to the same state every render. A different `state` call elsewhere in the code, however, would reference different state. 

The `state` hook takes the context `self` and a function providing the state's default value for `u64` and returns a tuple with a reference to the 
current state (which we name `count`) and a setter, which we name `set_count`. The setter has an `update` method which accepts a closure that receives a `&mut u64` and modifies it. Every time the user clicks on the button, `on_click` fires, calling `set_counter.update()`, which runs 
`*count += 1`, updating the state and causing the component to be rerendered. 

Note that the reference returned is _tracked_, so here we get `Tracked<&u64>` instead of `&u64`. This is where the tracked system becomes useful.
Constant values, when passed into components, are never considered updated. But if we called `set_count.updated` since the last time we called `state`,
on the next call of `state`, `count` will be marked as updated. That lets `avalanche` know we should update the text of `Text!(tracked!(count))` in the UI.

The state returned by each call site of `state` is unique. If we call `state` in two different places, their values aren't linked.

> If you've used React before, you might be familiar with the rules of hooks. Those don't apply here:
> feel free to call hooks within `if`, loops, and other complex control flow! You do still need to 
> keep hook calls inside of components, but that's enforced at compile time.

With that, we have our first stateful component! If we instead wanted to simply 
set the state to a value like `0`, we could write `set_count.set(0)` as a shorthand for `set_count.update(|count| *count = 0)`.

## Dynamic rendering

Oftentimes, we don't just want to update property values based on changes to state and props, but also what children we render.
In some frameworks, that requires special template syntax, but we can take advantage of the fact that the `Component!()` syntax returns plain 
Rust values to use normal control flow instead.

### Conditional rendering

Want to render something only if a condition is true? Use an `if` statement:

```rust
# use avalanche::{component, tracked, View};
# use avalanche_web::components::Text;
#
#[component]
fn Conditional(cond: bool) -> View {
    if tracked!(cond) {
        Text!("True!")
    } else {
        ().into()
    }
}
```

> From this point, we'll be omitting previously used `avalanche` and `avalanche_web` imports for brevity.

The `()` type is a special `Component` that renders nothing. We call `.into()` on it to turn it into a `View`.

There is also a shorthand for this some component-or-nothing case:

```rust
# use avalanche::{component, tracked, View};
# use avalanche_web::components::Text;
#
#[component]
fn Conditional(cond: bool) -> View {
    tracked!(cond).then(|| Text!("True!")).into()
}
```

That's equivalent to the above example. This uses [`bool`'s then method](https://doc.rust-lang.org/std/primitive.bool.html#method.then).
 `Option<View>` is also a special component, rendering the `View` in its `Some` case and rendering nothing in its `None` case. Of course, if you want more complex conditionals, you can use other control flow like `else if` and `match`.