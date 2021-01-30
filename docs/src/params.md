# Data and dynamic parameters

## Data

Component constructors can take expressions as parameters, not just literals:

```rust
import avalanche::{component, View};
import avalanche::components::A;

const href: &str = "http://info.cern.ch/hypertext/WWW/TheProject.html";

#[component]
fn Link() -> View {
    A! {
        href: href,
        child: Text! {text: "The first website"}
    }
}
```

In cases where a variable has the same name as a parameter, there's a shorthand available:

```rust
import avalanche::{component, View};
import avalanche::components::A;

const href: &str = "http://info.cern.ch/hypertext/WWW/TheProject.html";

#[component]
fn Link() -> View {
    A! {
        // same as above
        href,
        child: Text! {text: "The first website"}
    }
}
```

## Parameters and nesting

So far, we've dealt only with components with hard-coded data. However, within most practical components, we want to allow other components to pass it data.
To enable that, you simply need to add parameters to your component function. Here, let's say we want a reusable `Link` with its own custom functionality,
and want to allow a custom destination and text to be specified. We can simply just add `to` and `text` parameters:

```rust
import avalanche::{component, View};
import avalanche::components::A;

#[component]
fn Link(to: String, text: String) -> View {
    A! {
        href: to,
        child: Text! {text: text.clone()}
    }
}
```

Notice that for the text parameter, we pass `text.clone()` instead of `text`. That's because even though we specify `text` is a `String`,
we actually receive parameters as references (allowing a componen to be rerendered when its state but not props change). So `text` is really a `&String`.

We can then construct `Link` inside of other components:

```rust
# use avalanche::{component, View};
# import avalanche::components::A;
# 
# #[component]
# fn Link(to: String) -> View {
#     A! {
#         href: to
#     }
# }
use avalanche_web::{Text, Div};

fn Example() -> View {
    Div! {
        children: [
            Link! {
                to: "https://example.com".into(),
                text: "example.com".into()
            },
            Text! {text: " is a domain reserved for use in demos."}
        ]
    }
}
```

When rendered, the above is equivalent to the HTML below.
```html
<div>
    <a href="https://example.com">
        example.com
    </a>
    &nbsp;is a domain reserved for use in demos.
</div>
```

Within component constructors, parameter order does not matter, so we could've also written our `Link` construction with `to` and `text` swapped:
```
Link! {
    text: "example.com".into(),
    to: "https://example.com".into()
}
```

## Parameter type restrictions

You might have noticed that we used the `String` type for our parameters, rather than the more generic `&str`. That's because 
parameters are stored and need to live longer than the function body, and are thus required to be `'static`. That means
parameter types can't contain non-`-static` references. However, `static` references like `&'static str` are allowed.
