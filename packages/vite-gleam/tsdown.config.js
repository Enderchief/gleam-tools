import { defineConfig } from "tsdown";

export default defineConfig({
    entry: ["src/index.ts"],

    clean: true,
    target: 'es2022',
    format: ["esm", "cjs"],

    deps: {
        neverBundle: ['vite', 'esbuild', 'postcss', 'rollup', 'magic-string', 'toml']
    },
    outputOptions: {
        exports: 'named'
    },
    dts: {
        resolve: true,
        entry: "src/index.ts",
        skipLibCheck: true,
        compilerOptions: {
            moduleResolution: 'node'
        }
    }
})
