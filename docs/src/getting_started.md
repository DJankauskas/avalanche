# Getting started

## Prerequisites

Before you get started, ensure you have `cargo` and `npm` installed.

## Creating an app

To create an `avalanche_web` app, first install trunk if you haven't already:
`cargo install trunk`.

Next, create a binary application with `cargo new <my-app-name>`. Within the root of that 
directory, create an index.html file with these contents:
```html
<html>
  <head>
  </head>
  <body>
  </body>
</html>
```
This serves as a template for `trunk` to generate an entrypoint `index.html` file for 
your application, along with providing us scaffolding to supply app metadata and the like.

With that basic setup out of the way, it's time to actually integrate `avalanche-web`.
In the `Cargo.toml` file, add the following dependencies:

```toml
avalanche = "^0.1.0"
avalanche_web = "^0.1.0"
```

Next, add this code somewhere outside the `main` function:

```rust
use avalanche::{component, View};
use avalanche_web::components::{H1, Text};

#[component]
fn HelloWorld() -> View {
    H1(self, [
        Text(self, "Hello world!")
    ])
}
```

Finally, add this call in `main`:
```rust
# use avalanche::{component, View};
# use avalanche_web::components::{H1, Text};
# 
# #[component]
# fn HelloWorld() -> View {
#     H1(self, [
#         Text(self, "Hello world!")
#     ])
# }
# fn main() {
avalanche_web::mount_to_body::<HelloWorld>();
# }
```

Now, to see your app running in the browser, execute `trunk serve --open` within the 
`my-app-name` directory. If you haven't gotten any compiler errors,
you should see your first `avalanche` and `avalanche_web` web app!

## Explaining hello world

So what exactly is going on in the code above? Well, the most important aspect here is the `#[component]` attribute macro. 
`#[component]` accepts a function returning a `View`, which is the result of rendering a component. 
Within a function marked with `#[component]`, functions whose names begin with a capital ASCII character
and accept the component context `self` are interpreted as special component calls. 
`Text` is a component with a single parameter, so by specifying that, along with the context `self`, we get an instance 
of `View` with that text component. `H1` can take an array of children, so this component here is equivalent to writing
this raw HTML:
```html
<h1>
    Hello world!
</h1>
```

Finally, we pass our new `HelloWorld` component as a type argument to `avalanche_web::mount_to_body`, which, as the name implies, 
renders our `HelloWorld` component within the `<body></body>` of our web app.