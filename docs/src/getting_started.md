# Getting started

## Prerequisites

Before you get started, ensure you have `cargo` and `npm` installed.

## Creating an app

To create an `avalanche_web` app, first run the command `npm init rust-webpack my-app-name`, where `my-app-name` is the name
of the application you wish to create. This will create a directory with name `my-app-name`. Your Rust code will live within its `/src` directory.

Within `src/lib.rs`, you will see some helpful preloaded setup, including a `panic` hook for debugging messages, and the `main_js` function. To get a minimal `avalanche_web` app off the ground, first we need to add some dependencies. In the `Cargo.toml` file, add the following dependencies:

```toml
avalanche = { git = 'https://github.com/DJankauskas/avalanche/tree/master/avalanche' }
avalanche_web = { git = 'https://github.com/DJankauskas/avalanche/tree/master/avalanche_web` }
```

Next, add this code somewhere outside the `main_js` function:

```rust
use avalanche::{component, View};
use avalanche::components::{H1, Text};

#[component]
fn HelloWorld() -> View {
    H1 {
        child: Text! {text: "Hello world!"}
    }
}
```

Finally, add this call in `main_js`:
```rust
avalanche_web::mount_to_body::<HelloWorld>();
```

Now, to see your app, run `npm start` within the `my_app_name` directory. If you haven't gotten any compiler errors,
you should see your first `avalanche` and `avalanche_web` web!

## Explaining hello world

So what exactly is going on in the code above? Well, the most important aspect here is the `#[component]` attribute macro. 
`#[component]` accepts a function returning a `View`, which is a wrapper around an instance of a component. 
Within a function marked with `#[component]`, macros whose names begin with a capital ASCII character are interpreted as 
special component constructors. `Text` is a component with a single parameter `text`, so by specifying that we get an instance 
of `View` with that text component. `H1` can take either one `child` or an array of children, so this component here is equivalent to writing
this raw HTML:
```html
<h1>
    Hello world!
</h1>
```

Finally, we pass our new `HelloWorld` component as a type argument to `avalanche_web::mount_to_body`, which, as the name implies, 
renders our `HelloWorld` component within the `<body></body>` of our web app.