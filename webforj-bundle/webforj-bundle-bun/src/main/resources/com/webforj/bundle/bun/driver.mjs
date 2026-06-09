/**
 * Generated build driver for the webforJ bun bundler.
 *
 * @author Hyyan Abo Fakher
 */

import { watch } from 'node:fs';
import { writeFileSync, mkdirSync, readFileSync } from 'node:fs';
import { relative, dirname, resolve } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const cfg = JSON.parse(readFileSync(resolve(here, 'driver.config.json'), 'utf8'));

// Load the optional app bun.config.ts once. Its default export is extra plugins to append to the build.
const userMod = cfg.userConfig ? await import(pathToFileURL(cfg.userConfig).href) : {};
const pluginOptions = (userMod && userMod.options) || {};

// Each curated wrapper default-exports a factory. webforJ calls it with the project supplied options
// for that plugin id (or an empty object), then appends the app's own plugins.
const plugins = [];
for (const ref of cfg.plugins) {
  const factory = (await import(new URL('./plugins/' + ref.wrapper, import.meta.url).href)).default;
  plugins.push(factory(pluginOptions[ref.id] || {}));
}

if (userMod && Array.isArray(userMod.default)) {
  plugins.push(...userMod.default);
}

// Map an output back to the entry that produced it by comparing the path stem, not the output order:
// a CSS-only entry emits an asset rather than an entry-point, so the entry-point list does not align
// with the entries list. The stem of card/card-<hash>.js and its companion card/card-<hash>.css both
// equal the entry base card/card, while shared chunk-<hash>/asset-<hash> outputs match no entry. The
// `buildPath` is the file bun builds and `source` is the index key it is recorded under (for an npm
// entry the buildPath is a generated synthetic file and the source is the original specifier).
function entryBase(buildPath) {
  return buildPath.replace(/\.[^./]+$/, '');
}

function outputBase(rel, hashed) {
  let s = rel.replace(/\.[^./]+$/, '');
  if (hashed) {
    s = s.replace(/-[A-Za-z0-9]+$/, '');
  }

  return s;
}

const bases = new Map();
for (const ref of cfg.entries) {
  const base = entryBase(ref.buildPath);
  if (!bases.has(base)) {
    bases.set(base, ref.source);
  }
}

async function runBuild() {
  const result = await Bun.build({
    entrypoints: cfg.entries.map((e) => e.buildPath),
    outdir: cfg.outdir,
    root: cfg.root,
    target: 'browser',
    format: 'esm',
    splitting: cfg.splitting,
    naming: { entry: cfg.entryNaming, chunk: 'chunk-[hash].[ext]', asset: 'asset-[hash].[ext]' },
    minify: cfg.minify,
    plugins
  });

  if (!result.success) {
    for (const log of result.logs) {
      console.error(String(log));
    }
  }

  const outputs = {};
  for (const out of result.outputs) {
    const rel = relative(cfg.outdir, out.path).split('\\').join('/');
    const key = bases.get(outputBase(rel, cfg.hashed));
    outputs[rel] = key ? { entryPoint: key } : {};
  }

  mkdirSync(dirname(cfg.metafile), { recursive: true });
  writeFileSync(cfg.metafile, JSON.stringify({ outputs }));

  const count = cfg.entries.length;
  console.log('Bundled ' + count + ' entr' + (count === 1 ? 'y' : 'ies'));

  return result.success;
}

const watching = process.argv.includes('--watch');
const ok = await runBuild();
if (watching) {
  let timer;
  const watcher = watch(cfg.root, { recursive: true }, (event, file) => {
    if (file && file.split(/[\\/]/).includes('node_modules')) {
      return;
    }

    clearTimeout(timer);
    timer = setTimeout(() => {
      runBuild().catch((e) => console.error(String(e)));
    }, 60);
  });

  const stop = () => {
    try {
      watcher.close();
    } catch (e) {
      // closing on shutdown, ignore
    }

    process.exit(0);
  };

  process.on('SIGINT', stop);
  process.on('SIGTERM', stop);
} else if (!ok) {
  process.exit(1);
}
