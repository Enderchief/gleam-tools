import { defineConfig } from 'vite';
import Inspect from 'vite-plugin-inspect';
import gleamVite from 'vite-gleam';

export default defineConfig({
  plugins: [gleamVite({ dependencies: ['gleam_stdlib'] }), Inspect()],
});
