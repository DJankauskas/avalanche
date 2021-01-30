# Event handling and input elements

So far, we've only used the `on_click` event, but there are both many more events and more functionality available.

In addition, many events are component-specific, like `on_input` and `on_blur` on some form elements. many of these are available,
but some have not been implemented yet. If you're missing one, please [file an issue!](https://github.com/DJankauskas/avalanche/issues/new)

## Events

[Every global event](https://developer.mozilla.org/en-US/docs/Web/API/GlobalEventHandlers) is settable as a parameter
on every non-`Text` `avalanche_web` component. Each event handling parameter takes a function of type `impl Fn(TypedEvent<E, C>)`
where `E` is the `web_sys` type for the handler's event type and `C` is `web_sys`'s native type for the component's native `web_sys` type.

Often, we don't need the event, so we omit it, hence closures like `on_click: move |_| ...`.

## Input elements

One case where events are useful is with input elements. That's because the `TypedEvent` type gives access to the `current_target()` method,
which provides a reference to the component's associated native reference. We can use this to keep track of an `Input` element's state:

```rust
# use avalanche::{component, View, UseState};
# use avalanche_web::components::{Input};
#
#[component(text = UseState<String>)]
fn ControlledInput() -> View {
    let (text, set_text) = text(String::new());

    Input! {
        value: Some(text.clone()),
        on_input: move |e| set_text.call(|text| *text = e.current_target().unwrap().value())
    }
}
```

In this example, `text` holds the input's current contents, allowing us to use it for other purposes.

> In React, programmers often use `on_change` instead of `on_input`, but React semantics do not match native browser ones in this case;
> use `on_input` in `avalanche_web` instead.