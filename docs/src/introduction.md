# Introduction

## What is avalanche?

`avalanche` is a platform-agnostic UI library aiming to bridge the strong typing, efficient runtime performance, and 
cross-platform prowess of Rust with the ergonomics of existing UI libraries like Svelte. 

Like React and Vue, this library allows writing your UI code declaratively, with the ease-of-use of HTML
but with dynamic updates. Unlike those libraries, however, `avalanche` analyzes your code
at compile time to eliminate the need for most runtime comparisons and updates, meaning you get declarative
ergonomics with imperative performance.

While we believe the foundations for these goals are strong, the library is still very much a work in progress,
and the implementation may not always reach our standards yet. As such, while this library is useable for hobbyist 
projects, we **DO NOT** yet recommend you use this library in production.

## What is avalanche_web?

`avalanche_web` is the official Web platform interface for `avalanche`, allowing you to use `avalanche` in browsers and
Electron apps by compiling your app to WebAssembly. It may be missing some listeners and other functionality as of now, but all
HTML tags are available for use as components.

## Using this tutorial

Before using this tutorial, you should first have a decent grasp of the Rust programming language. This tutorial also heavily uses `avalanche_web`, so you should be familiar with basic HTML.

This tutorial is new and still rough, so if you have any confusion or see a potential error, we'd be grateful if you
[created an issue](https://github.com/DJankauskas/avalanche/issues).

## What are components?

Components are encapsulated pieces of UI, potentially with complex logic and state, that can be reused as desired. An `avalanche` app always contains a root component, and can be built out of many more.