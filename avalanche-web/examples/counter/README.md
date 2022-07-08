## Prerequisites

```sh
cargo install trunk
```

## How to run in debug mode

```sh
# Builds the project and serves it on port 8080 by default. Auto-reloads when the project changes.
trunk serve
```

## How to build in release mode

```sh
# Builds the project and places it into the `dist` folder.
trunk build --release
```