import { compile as c } from "../gleam.mjs";
import { lstat, readFile, readdir } from "node:fs/promises";
import { resolve, relative, join } from "node:path";

import type { Plugin } from "vite";
import type { Options } from "../types";

async function compile(
  path?: string,
  dependencies?: string[],
): Promise<{ Ok?: Map<string, string>; Err?: string }> {
  const source = await readDeps();

  if (path) {
    const rel = "./" + relative(resolve("."), path);

    const file = await readFile(path, { encoding: "utf8" });
    const toml_path = "./gleam.toml";

    const toml = await readFile(toml_path, {
      encoding: "utf8",
    });

    source["/gleam.toml"] = toml;
    source["/" + rel.slice(1)] = file;
  }

  const res: { Ok?: Map<string, string>; Err?: string } = await c({
    target: "javascript",
    mode: "Dev",
    dependencies: dependencies ?? [],
    sourceFiles: source,
  });
  if (res.Ok) {
    const n = new Map();
    res.Ok.forEach((v, k) => {
      n.set(
        k.replace(/\/build\/packages\/(.+)\/src/, "/build/dev/javascript/$1"),
        v,
      );
    });
    res.Ok = n;
  }
  return res;
}

let cached: Map<string, string> = null;

export async function readDeps(BASE_PATH = "./build/packages") {
  let stat = await lstat(BASE_PATH);
  if (!stat.isDirectory()) return;
  return makeDeps(BASE_PATH, {});
}

async function makeDeps(path: string, o: Record<string, string> = {}) {
  const stat = await lstat(path);
  if (stat.isDirectory()) {
    await Promise.allSettled(
      (await readdir(path)).map(async (p) => await makeDeps(`${path}/${p}`, o)),
    );
  } else if (stat.isFile()) {
    const f = await readFile(path, { encoding: "utf8" });
    o[path.slice(1)] = f;
  }

  return o;
}

export default async function gleamVite(options: Options) {
  const res = (await compile(null, options.dependencies)).Ok;
  if (res) cached = res;

  return {
    name: "gleam",
    async load(id) {
      if (id.startsWith("gleam:")) {
        id = id.replace("gleam:", "");
        const file = cached.get(id);

        if (file) return { code: file };
      }

      if (!id.endsWith(".gleam")) return;

      const res = await compile(id, options.dependencies);

      if (!res.Ok) return;

      cached = res.Ok;

      const filename = relative(resolve("."), id)
        .replace(/src\/(.+)/, "$1")
        .replace(".gleam", ".mjs");

      const code = res.Ok.get(`/build/dev/javascript/gleam-wasm/${filename}`);

      return {
        code,
      };
    },
    resolveId(source, importer) {
      if (importer.endsWith(".gleam")) {
        const start = resolve("./src");

        importer = importer.replace(start, "/build/dev/javascript/gleam-wasm");

        const path = join(importer, "..", source);

        return { id: `gleam:${path}` };
      } else if (importer.startsWith("gleam:")) {
        importer = importer.replace("gleam:", "");

        const path = join(importer, "..", source);
        return { id: `gleam:${path}` };
      }
    },
  } satisfies Plugin;
}
