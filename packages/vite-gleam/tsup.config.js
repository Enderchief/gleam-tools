import { defineConfig } from "tsup";

export default defineConfig({
    entry: ["src/index.ts"],

    clean: true,
    target: 'es2022',
    format: ["esm", "cjs"],

    bundle: true,
    dts: {
        resolve: true,
        entry: "src/index.ts",
        compilerOptions: {
            moduleResolution: 'node'
        }
    }
})
