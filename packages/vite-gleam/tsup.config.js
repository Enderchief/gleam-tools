import { defineConfig } from "tsup";

export default defineConfig({
    entry: ["src/index.ts"],

    clean: true,
    target: 'es2022',
    format: ["esm"],
    bundle: false,
})
