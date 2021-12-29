<h1 align="center">Avalanche</h1>
<h4 align="center">
    <a href="https://djankauskas.github.io/avalanche/">Book</a>
    <span> | </span>
    <a href="https://docs.rs/avalanche/0.1.0/avalanche/">Docs</a>
    <span> | </span>
    <a href="https://github.com/DJankauskas/avalanche/tree/master/avalanche-web/examples">Examples</a>
    <span> | </span>
    <a href="https://github.com/DJankauskas/avalanche/blob/master/ROADMAP.md">Roadmap</a>
</h4>

Avalanche is a performant library for building declarative, performant UIs. It analyzes functional component definitions at compile time to generate 
efficient and precise app updates. It is bundled with avalanche-web, which provides facilities and components for building WebAssembly web apps. This library is
in early stages of development, and should not yet be used in production. 

This crate does not respect semver for `0.1.x`. 

## How it works
Functions marked with `#[component]` define UI declaratively by returning other components. Component parameters and state
are augmented by tracking, a system where avalanche tracks whether variables have been updated, only updating the UI for changed
data. This allows for higher performance while keeping code ergonomic. 

## Getting started

To learn the library, we highly recommend you check out the [avalanche book](https://djankauskas.github.io/avalanche/), which gives a primer on
creating and building a web app using avalanche and avalanche web. The [todomvc example](https://github.com/DJankauskas/avalanche/tree/master/avalanche-web/examples/todomvc)
is a great example app for getting a feel for basic avalanche idioms.

## License

Licensed under either of

 * Apache License, Version 2.0
   [LICENSE-APACHE](LICENSE-APACHE)
 * MIT license
   [LICENSE-MIT](LICENSE-MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
