# ts-gleam [![npm](https://img.shields.io/npm/v/ts-gleam)](https://npmjs.com/ckage/ts-gleam)

TypeScript LSP Plugin for importing [Gleam](https://gleam.run) files.

## Usage

1. Create a new Gleam project.
2. In `gleam.toml`, set `target=javascript` and under `[javascript]` set `typescript_declarations=true`.
3. `npm add ts-gleam`
4. Create a `tsconfig.json`/`jsconfig.json` and set `compilerOptions.plugins` to `[{"name": "ts-gleam"}]`
5. Build your Gleam project when ever you have changes and import away!

## Note

This **does not** build and resolve imports for your. All this is, is for the LSP to type check correctly.  
For building with Gleam/JavaScript, check out [`vite-gleam`](https://github.com/Enderchief/gleam-tools/tree/master/packages/vite-gleam) or to quickly scaffold an app try [`create-gleam`](https://github.com/Enderchief/create-gleam).
