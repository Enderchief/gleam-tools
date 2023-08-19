# vite-gleam [![npm](https://img.shields.io/npm/v/vite-gleam)](https://npmjs.com/package/vite-gleam)

Import from [Gleam](https://gleam.run/) (`*.gleam`) files directly.

## Usage

1. `npm i vite-gleam`
2. Create a basic Vite project (`npm create vite`)
3. Create a `gleam.toml`
4. Add dependencies with `gleam add`
5. Update your vite config

```ts
// vite.config.{ts,js}
import gleam from 'vite-gleam';

export default {
  plugins: [
    gleam({
      // options passed here
    }),
  ],
};
```

```ts
interface Options {
  // array of packages added via `gleam add`
  dependencies: string[];
}
```

6. Start importing from Gleam!

## Note (read all of this)

- `*.gleam` can be imported from any file and vice-verca (js, ts, jsx, vue, svelte, html, etc). The only exception to this is importing a `*.gleam` file from another `*.gleam` file.
- Packages from `hex` can be imported from Gleam provided that the are added via `gleam add` and added to the dependencies array in options.
- By default, TypeScript (LSP) will complain about importing files with the `.gleam` extension. To fix this, add `declare module "*.gleam";` inside any TypeScript file. A caveat is the LSP does not know if a export exists so it will not provide autocomplete when importing a Gleam file and it will type exports as `any`.
