import fs from "node:fs";
import path from "node:path";
import { JsonArray, JsonMap, parse } from "@iarna/toml";
import type tsModule from "typescript/lib/tsserverlibrary.js";

const GLEAM_REGEX = /\.gleam/;

function _isGleam(fileName: string): boolean {
  return GLEAM_REGEX.test(fileName);
}

function _hasDeclaration(
  fileName: string,
  projectName: string,
  logger: tsModule.server.Logger,
) {
  const _filepath = fileName
    .replace("/src/", `/build/dev/javascript/${projectName}/`)
    .replace(/\.gleam/, ".d.mts");
  logger.info(`[ts-gleam] checking if declaration exists at "${_filepath}"`);
  return fs.existsSync(_filepath);
}

function getDtsSnapshot(
  ts: typeof tsModule,
  projectName: string,
  fileName: string,
  logger: tsModule.server.Logger,
): tsModule.IScriptSnapshot {
  const _filepath = fileName
    .replace("/src/", `/build/dev/javascript/${projectName}/`)
    .replace(/\.gleam/, ".d.mts");
  logger.info(`[ts-gleam] loading declaration from ${_filepath}`);
  const _file = fs.readFileSync(_filepath, { encoding: "utf-8" });
  const _dts = ts.ScriptSnapshot.fromString(_file);
  return _dts;
}

function init(modules: {
  typescript: typeof import("typescript/lib/tsserverlibrary.js");
}) {
  const ts = modules.typescript;

  function create(info: tsModule.server.PluginCreateInfo) {
    const logger = info.project.projectService.logger;

    logger.info("[ts-gleam] initializing ts-gleam");

    const directory = info.project.getCurrentDirectory();
    process.chdir(directory);

    const languageServiceHost = {} as Partial<tsModule.LanguageServiceHost>;
    const languageServiceHostProxy = new Proxy(info.languageServiceHost, {
      get(target, key: keyof tsModule.LanguageServiceHost) {
        return languageServiceHost[key]
          ? languageServiceHost[key]
          : target[key];
      },
    });

    const languageService = ts.createLanguageService(languageServiceHostProxy);

    let projectName: string = "theta";
    const _gleam_toml_path = path.join(directory, "./gleam.toml");
    if (!fs.existsSync(_gleam_toml_path)) {
      debugger;
      logger.info("[ts-gleam] ERROR | gleam.toml does not exist");
      return languageService;
    }
    const _gleam_toml_raw = fs.readFileSync(_gleam_toml_path, {
      encoding: "utf-8",
    });

    let _gleam_toml: JsonMap;
    try {
      _gleam_toml = parse(_gleam_toml_raw);
    } catch (e) {
      logger.info(`[ts-gleam] ERROR | ${e}`);
      return languageService;
    }

    const _name = _gleam_toml["name"];
    logger.info(`[ts-gleam] name is: "${_name}"`);
    if (typeof _name === "string") projectName = _name;
    else {
      logger.info(`[ts-gleam] ERROR | name does not exist`);
      return languageService;
    }
    if (
      typeof _gleam_toml.javascript !== "object" ||
      (<JsonMap>_gleam_toml.javascript).typescript_declarations !== true
    ) {
      logger.info("[ts-gleam] ERROR | typescript declarations not enabled");
      return languageService;
    }

    languageServiceHost.getScriptKind = (fileName) => {
      if (!info.languageServiceHost.getScriptKind) {
        return ts.ScriptKind.Unknown;
      }
      if (
        _isGleam(fileName) &&
        _hasDeclaration(fileName, projectName, logger)
      ) {
        return ts.ScriptKind.TS;
      }
      return info.languageServiceHost.getScriptKind(fileName);
    };

    languageServiceHost.getScriptSnapshot = (fileName) => {
      if (
        _isGleam(fileName) &&
        _hasDeclaration(fileName, projectName, logger)
      ) {
        logger.info(`[ts-gleam] current: ${fileName}`);
        const dts = getDtsSnapshot(ts, projectName, fileName, logger);
        return dts;
      }
      return info.languageServiceHost.getScriptSnapshot(fileName);
    };

    function createModuleResolver(containingFile: string) {
      return (
        moduleName: tsModule.StringLiteralLike,
        resolveModule: () =>
          | tsModule.ResolvedModuleWithFailedLookupLocations
          | undefined,
      ): tsModule.ResolvedModuleFull | undefined => {
        const p = path.resolve(path.dirname(containingFile), moduleName.text);
        if (_isGleam(moduleName.text)) {
          return {
            extension: ts.Extension.Dts,
            isExternalLibraryImport: false,
            resolvedFileName: p,
          };
        }
      };
    }

    if (info.languageServiceHost.resolveModuleNameLiterals) {
      const _resolveModuleNameLiterals =
        info.languageServiceHost.resolveModuleNameLiterals!.bind(
          info.languageServiceHost,
        );
      languageServiceHost.resolveModuleNameLiterals = (
        modulesLiterals,
        containingFile,
        ...rest
      ) => {
        const resolvedModules = _resolveModuleNameLiterals(
          modulesLiterals,
          containingFile,
          ...rest,
        );

        const moduleResolver = createModuleResolver(containingFile);

        return modulesLiterals.map((moduleName, index) => {
          try {
            const resolvedModule = moduleResolver(
              moduleName,
              () =>
                languageServiceHost.getResolvedModuleWithFailedLookupLocationsFromCache?.(
                  moduleName.text,
                  containingFile,
                ),
            );

            if (resolvedModule) return { resolvedModule };
          } catch (e) {
            logger.info(`[ts-gleam] ERR: ${e}`);
            return resolvedModules[index];
          }
          return resolvedModules[index];
        });
      };
    }

    return languageService;
  }

  function getExternalFiles(
    project: tsModule.server.ConfiguredProject,
  ): string[] {
    return project.getFileNames().filter(_isGleam);
  }

  return { create, getExternalFiles };
}

export = init;
