<h1 align="center">Avalanche</h1>
<h4 align="center">
    <a href="https://djankauskas.github.io/avalanche/">Book</a>
    <span> | </span>
    <a href="https://github.com/DJankauskas/avalanche/tree/master/avalanche-web/examples">Examples</a>
    <span> | </span>
    <a href="https://github.com/DJankauskas/avalanche/blob/master/ROADMAP.md">Roadmap</a>
</h4>

Avalanche is a performant library for building declarative, performant UIs. It analyzes functional component definitions at compile time to generate 
efficient and precise app updates. It is bundled with avalanche web, which provides facilities and components for building WebAssembly web apps. This library is
in early stages of development, and should not yet be used in production. 

## How it works
Functions marked with `#[component]` are analyzed to determine what properties and instances of state might influence a child's property. If any of those have changed, that
property will be marked as updated, while unmarked properties will be ignored (except on first render).

## Getting started

To learn the library, we highly recommend you check out the [avalanche book](https://djankauskas.github.io/avalanche/), which gives a primer on
creating and building a web app using avalanche and avalanche web. The [todomvc example](https://github.com/DJankauskas/avalanche/tree/master/avalanche-web/examples/todomvc)
is a great example app for getting a feel for basic avalanche idioms.
