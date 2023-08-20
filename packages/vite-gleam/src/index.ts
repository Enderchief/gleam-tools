import { lstat, readFile, readdir, rename } from "node:fs/promises";

import type { Plugin } from "vite";
import { execSync } from "node:child_process";
import { parse } from "toml";
import { resolve, relative, join } from "node:path";
import MagicString from "magic-string";

interface GleamConfig {
  name: string;
  version: string;
  target: string;
}

let gleam_config: GleamConfig | undefined = undefined;

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

export async function build() {
  if (!gleam_config) throw new Error("gleam.toml not found");
  console.log("$ gleam build");
  const out = execSync("gleam build", { encoding: "utf8" });
  console.log(out);

  const path = `./build/dev/javascript/${gleam_config?.name}`;
  const all_ts = (await readdir(path)).map(async (v) => {
    const stat = await lstat(v);
    if (stat.isFile() && v.endsWith(".d.ts")) {
      await rename(v, v.replace(".d.ts", ".gleam.d.ts"));
    }
  });
  await Promise.allSettled(all_ts);
}

export function jsPath(id: string): string {
  id = id.replace(".gleam", ".mjs");

  let path = relative(resolve("."), id);
  if (path.startsWith("src")) {
    path = path.replace("src/", `${gleam_config?.name}/`);
  }

  return path;
}

export default async function gleamVite(): Promise<Plugin> {
  return {
    name: "gleam",
    async buildStart() {
      const toml_exist = await lstat("./gleam.toml");
      if (!toml_exist.isFile()) throw Error("gleam.toml not found");
      const file = await readFile("./gleam.toml", { encoding: "utf8" });
      gleam_config = parse(file) as GleamConfig;

      await build();
    },
    async resolveId(source, importer) {
      if (!importer || !importer.endsWith(".gleam")) return;

      importer = jsPath(importer);

      const path = join(
        resolve("."),
        "/build/dev/javascript/",
        importer,
        "..",
        source,
      );
      return {
        id: path,
      };
    },
    async transform(code, id) {
      if (!id.endsWith(".gleam")) return;

      const path = id.replace(".gleam", ".mjs");

      const file = await readFile(`./build/dev/javascript/${jsPath(path)}`, {
        encoding: "utf8",
      });

      const s = new MagicString(code);
      s.overwrite(0, code.length - 1, file);

      const map = s.generateMap({ source: id, includeContent: true });

      return {
        code: file,
        map: map,
      };
    },
    async handleHotUpdate(ctx) {
      if (ctx.file.endsWith(".gleam")) await build();
    },
  } satisfies Plugin;
}
