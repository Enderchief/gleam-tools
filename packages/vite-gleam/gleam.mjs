import { init, compile as c } from "./gleam/gleam_wasm.js";

init(false);

export function compile(options) {
    return c(options);
}

