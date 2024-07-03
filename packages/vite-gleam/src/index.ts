import { lstat, readdir, readFile } from "node:fs/promises";

import type { Plugin } from "vite";
import { execSync } from "node:child_process";
import { parse } from "toml";
import { join, relative, resolve, sep } from "node:path";
import MagicString from "magic-string";

interface GleamConfig {
  name: string;
  version: string;
  target: string;
}

let gleam_config: GleamConfig | undefined = undefined;

export async function readDeps(BASE_PATH = `.${sep}build${sep}packages`) {
  let stat = await lstat(BASE_PATH);
  if (!stat.isDirectory()) return;
  return makeDeps(BASE_PATH, {});
}

async function makeDeps(path: string, o: Record<string, string> = {}) {
  const stat = await lstat(path);
  if (stat.isDirectory()) {
    await Promise.allSettled(
      (await readdir(path)).map(
        async (p) => await makeDeps(`${path}${sep}${p}`, o),
      ),
    );
  } else if (stat.isFile()) {
    const f = await readFile(path, { encoding: "utf8" });
    o[path.slice(1)] = f;
  }

  return o;
}

export async function build() {
  if (!gleam_config) throw new Error("gleam.toml not found");
  console.log("$ gleam build --target=javascript");
  const out = execSync("gleam build --target=javascript", { encoding: "utf8" });
  console.log(out);
}

export function jsPath(id: string): string {
  id = id.replace(".gleam", ".mjs");

  let path = relative(resolve("."), id);
  if (path.startsWith("src")) {
    path = path.replace(`src${sep}`, `${gleam_config?.name}${sep}`);
  }

  return path;
}

export default async function gleamVite(): Promise<Plugin> {
  return {
    name: "gleam",
    config(config, env) {
      config.build ||= {};
      if (config.build.watch === null || config.build.watch === undefined)
        return;
      if (typeof config.build.watch !== "object") config.build.watch = {};
      let origin = config.build.watch!.exclude;
      if (!origin) origin = [];
      else if (typeof origin !== "object") origin = [origin];

      (<string[]>origin).push("build/**");
      config.build.watch!.exclude = origin;
    },
    async buildStart() {
      const toml_exist = await lstat(`.${sep}gleam.toml`);
      if (!toml_exist.isFile()) throw Error("gleam.toml not found");
      const file = await readFile(`.${sep}gleam.toml`, { encoding: "utf8" });
      gleam_config = parse(file) as GleamConfig;

      await build();
    },
    async resolveId(source, importer) {
      if (!importer) return;
      else if (source.startsWith("hex:")) {
        const path = join(
          resolve("."),
          `${sep}build${sep}dev${sep}javascript`,
          source.slice(4),
        );
        return { id: path };
      }

      if (!importer.endsWith(".gleam") && !source.endsWith("gleam.mjs")) return;

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

      const file = await readFile(
        `.${sep}build${sep}dev${sep}javascript${sep}${jsPath(path)}`,
        {
          encoding: "utf8",
        },
      );

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
