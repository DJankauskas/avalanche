# Tracking

The previous chapter showed how even with state, we can write our UI declaratively. But how, exactly, does this work?
In theory, `avalanche` could rerender our entire app on every state change, but in practice, that's often too inefficient.
Instead, `avalanche` uses the tracked system. Parameters and many hook return values are `Tracked`, and expressions with
`tracked!()` calls also have a value of type `Tracked`. Then, avalanche only marks child parameters as updated if their input 
had a `tracked!()` value that was updated.

## Handling impurity

The tracked system relies on _purity_, or the idea that a component's output should only be the result of its input parameters. 
Not doing that will lead to some issues. Take this first naive, impure attempt at introducing a random number to a 
component, using the `rand` crate's `random` function, which uses thread-local state in its implementation. 

Here, we'll try to show a random number, and generate a new one every time we click a button.

```rust
# use avalanche::{component, state, View};
# use avalanche_web::components::{Text, Div, Button};
use rand::random;

#[component]
fn Random() -> View {
    let (_, trigger_update) = state(self, || ());

    Div(
        self,
        [
            Button(
                self,
                on_click = move |_| trigger_update.set(()),
                Text(self, "Generate new number"),
            ),
            Div(self, [
                Text(self, random::<u16>().to_string())
            ]) 
        ]
    )
}
```
However, if you try out the example for yourself, you'll notice that the number doesn't actually update. A reasonable explanation for this would be
we're not actually modifying `trigger_update`'s state, or that its current value is equal to the previous one. However, calling a state setter _always_ 
triggers a component rerender. Rather, the reason is that `random::<u16>()` is not `Tracked`, so it's considered a constant. This leads to an important rule: 
a component's parameter will only update if it has a `tracked!()` call with an updated `Tracked` value. `Text(self, random::<u16>().to_string())` doesn't have a `tracked!()`
call, so it will never update.

The issue here is that `random` is an impure function, but avalanche doesn't know that. The solution is to use hooks to contain impurity, like we did with 
the counter example.

```rust
# use avalanche::{component, tracked, state, View};
# use avalanche_web::components::{Text, Div, Button};
use rand::random;

#[component]
fn Random() -> View {
    let (value, set_value) = state(self, || random::<u16>());

    Div(
        self,
        [
            Button(
                self,
                on_click = move |_| set_value.set(random()),
                Text(self, "Generate new number"),
            ),
            Div(self, [
                Text(self, tracked!(value).to_string())
            ]) 
        ]
    )
}
```

Now, `text` clearly depends on the `value` state, so when we call `set_value`, `avalanche` will update that text value on screen.
Using hooks introduces impurity in a manner avalanche can understand.

## Rules of tracked

This example leads us to the three main tracked rules:


### Avoid mutable variables in components

Mutable variables make the tracked system leaky. Consider this snippet of code:

```rust,ignore
let (value, _) = state(self, || 5);
let mut mutable = 2;
mutable += *tracked!(value);
```

Since `mutable` isn't tracked, but its value depends on a `Tracked` value, using it as a component parameter will 
cause unexpected failures to update UI appearance. Instead, prefer creating new variables.

### Wrap interior mutability and hidden side effects

For parameter values, directly using values like `rand::random()` is a problem because they depend on some external state
that `avalanche` is not aware of, leading to stale values displayed on rerender. This also applies to values like `Rc<RefCell<T>>`
if they are updated outside of hook functionality. When using interior mutability, then, make sure to wrap it in `state`, and perform updates 
within the `set` or `update` methods.

### Avoid third-party macros

Macros can lead to convenient code; for example, `Text(self, format!("Hello, {} {}!", tracked!(title), tracked!(name)))` is a lot clearer than macro-free
alternatives. However, when using non-`std` and `avalanche` macros, `#[component]` is unable to track their dependencies correctly, meaning
parameters based on those macros may not update correctly. In the future, we may instead opt to consider those macros always updated (at the cost of significantly reduced efficiency), or offer some syntax to specify tracked values explicitly for them. Either way, we recommend you avoid those macros for now. 

## Rendering dynamic lists

Rendering more heavily dynamic content is where the tracked system shines. But first, a pitfall:
here's a potential first attempt at rendering a list of dynamic children:

```rust
# use avalanche::{component, tracked, state, View};
# use avalanche_web::components::{Ul, Li, Text};
#
#[component]
fn List() -> View {
    let (items, update_items) = state(self, || vec!["a".to_string()]);
    Ul(
        self,
        tracked!(items).iter().map(|item| Li(
            self,
            key = item,
            Text(self, item)
        ))
    )
}
```

Here, we use the standard iterator method `map` and `collect` to create an iterator
producing `View`s from a `Vec` of `String`s. `avalanche_web` elements can take any
type implementing `IntoIterator<Item = View>` as children. 
Both iterators and `View` implement this trait. This allows us to pass 
both our iterator mapping strings to views, and `Text(self, item)` as children to
`Ul` and `Li` in this example.

However, notice that in the component `Text(self, item)`, the implicit `text` parameter has no `tracked!` call, so if we later 
update the element `"a"`, for example, that change won't be appropriately rendered. We can change that with the `store` hook,
which enables storing state with fine-grained tracking. What that means is we can keep track of when specific elements of the
state were last updated. This is possible with the `Tracked::new` method, which allows creating a tracked value with its wrapped
value and what render cycle, or `Gen`, it was created on. The init function and `update` method of `store` provide a `Gen`, allowing
us to create and modify tracked values.

```rust
# use avalanche::{component, tracked, View};
# use avalanche_web::components::{Ul, Li, Text};
use avalanche::{store, Tracked};

#[component]
fn List() -> View {
    let (items, update_items) = store(self, |gen| vec![Tracked::new("a", gen)]);

    Ul(
        self,
        tracked!(items).iter().map(|item| Li(
            self,
            key = tracked!(item),
            Text(self, tracked!(item))
        ))
    )
}
```
Now the `iter` method on `items` returns elements of type `Tracked<&String>` rather than `&String`.


### Keys

So far, this explanation has only applied previous concepts, but there's the important new concept of keys. Every avalanche component has a special `String` `key` parameter. Whenever a particular component call location in code may be called more than once, it is required to specify a key to disambiguate the multiple instantiations; this is enforced with a runtime panic if not specified. Note that the key must be added on the topmost level, so in our example above,
```rust,ignore
Li(
    self,
    key = tracked!(item),
    Text(self, tracked!(item))
)
```
is good but this is not:
```rust,should_panic,ignore,
Li(self, 
    Text(
        self,
        key = tracked!(item),
        tracked!(item)
    )
)
```