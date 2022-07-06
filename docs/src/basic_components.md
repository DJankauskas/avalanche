# Basic components

Components are the building blocks of `avalanche` apps.
They're represented as functions since they're fundamentally meant to be pure:
they take in inputs and describe a UI as their output. They're so fundamental that
this book is largely a component tutorial. 

## Creating components

Though components are written as functions, `avalanche` implements some quality-of-life and performance features
not possible with functions. This requires the use of specialized syntax.
If we call a component that takes no parameters called `Comp`, for example, we'll call
it with `Comp(self)`. These component calls only work within the bodies of other components, and they return the 
`View` type.


## Passing parameters to components

Most components have parameters, and they're named. For example, in the components below, we pass `href` by name. However, the last 
parameter in a function definition can be supplied without a name as the last parameter in a call. 
For `Text`, that's the `text` parameter, which takes something convertible to a `Cow<'_, str>`, 
meaning you can pass it either a `String` or a `&str`. 
For every other `avalanche_web` component, the unnamed last parameter is `children`, which takes an `Into<Vec<View>>`.
Below are two versions of a link component. One passes `text` and `children` explicitly, while the other does it implicitly, as is idiomatic. 

```rust
use avalanche::{component, View};
use avalanche_web::components::{A, Text};

const FIRST_SITE: &str = "http://info.cern.ch/hypertext/WWW/TheProject.html";

#[component]
fn LinkExplicit() -> View {
    A(
        self,
        href = FIRST_SITE,
        children = [
            Text(self, text = "The first website")
        ]
    )
}

#[component]
fn LinkImplicit() -> View {
    A(
        self,
        href = FIRST_SITE,
        [
            Text(self, "The first website")
        ]
    )
}
```

For `avalanche_web` components, it's generally recommended to use implicit last parameters, so we'll be using them from now on in this tutorial.
In third-party components, by convention, you should only use them if there is only one 
parameter, like in `Text`, or if the last parameter is a child or children of the component.

## Receiving parameters and tracking

So far, we've dealt only with components with hard-coded data. However, within most practical components, we want to allow other components to pass them data.
To enable that, you simply need to add parameters to your component function. Here, let's say we want a reusable `Link` with its own custom functionality,
and want to allow a custom destination and text to be specified. We can add `to` and `text` parameters:

```rust
use avalanche::{component, tracked, View};
use avalanche_web::components::{A, Text};

#[component]
fn Link(to: &str, text: &str) -> View {
    A(
        self,
        href = tracked!(to),
        [
            Text(self, tracked!(text))
        ]
    )
}
```

Notice that for the `href` and implicit `text` parameters, we pass `tracked!` parameters instead of just passing them by value.
That's because when we say a parameter is, for example, a `&str`, we actually receive a `Tracked<&str>`. A `Tracked` value 
wraps an inner value with data on whether it's been updated since last render. `avalanche` uses this to allow for efficient re-rendering
of components. Calling `tracked!` on a `Tracked` value gives us its inner value. Since `Text` and `A` expect `&str`-like values, but 
the `to` and `text` parameters are `Tracked<&str>`, we use `tracked!()` to get `&str` values.

We can then construct `Link` inside of other components:

```rust
use avalanche::{component, tracked, View};
use avalanche_web::components::{Text, Div};
# use avalanche_web::components::A;
# 
# #[component]
# fn Link(to: &str, text: &str) -> View {
#     A(
#         self,
#         href = tracked!(to),
#         [
#             Text(self, tracked!(text))
#         ]
#     )
# }

#[component]
fn Example() -> View {
    Div(self, [
        Link(
            self,
            to = "https://example.com",
            text = "example.com"
        ),
        Text(self, " is a domain reserved for use in demos.")
    ])
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

Within component calls, parameter order does not matter (except for what the implict last parameter is), 
so we could've also called `Link` with `to` and `text` swapped:
```rust,ignore
Link(
    self,
    text = "example.com".into(),
    to = "https://example.com".into()
)
```

## Parameter type restrictions

All parameters must implement `Clone`. Note that all non-`mut` references implement `Clone`, making this restriction
generally easy to deal with in practice.