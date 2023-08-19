import type { Plugin } from 'vite';

/**
 * @param id - Path to Gleam source
 * @param source - Gleam source
 */
export type Preprocesser = (
  id: string,
  source: string
) => { code: string } | null;

export interface Options {
  /** list of gleam dependencies from hex.pm */
  dependencies: string[];
  /** optional preprocesser(s) to have custom transforms on Gleam code*/
  preprocess?: Preprocesser | Preprocesser[];
}

export default function (options: Options): Promise<Plugin>;
export function readDeps(): Promise<Record<string, string>>;
