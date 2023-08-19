import { defineConfig } from 'vite';
import Inspect from 'vite-plugin-inspect';
import gleam from 'vite-gleam';

export default defineConfig({
  plugins: [gleam(), Inspect()],
  build: { sourcemap: true },
});
