/**
 * Minifies webforJ client-side resources.
 *
 * Usage: node scripts/minify.js
 *
 * @author Hyyan Abo Fakher
 * @since 25.11
 */
const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const BANNER = `/*!
 * Built by webforJ
 * Copyright BASIS International Ltd.
 */
`;

const HTML_MINIFIER = 'html-minifier-terser@7.2.0';
const ESBUILD = 'esbuild@0.27.1';

const htmlFiles = [
  {
    module: 'webforj-mcp-apps',
    input: 'META-INF/mcp/app.html',
    output: 'META-INF/mcp/app.min.html'
  }
];

const files = [
  {
    module: 'webforj-foundation',
    input: 'META-INF/resources/webforj/view-transitions.css',
    output: 'META-INF/resources/webforj/view-transitions.min.css'
  },
  {
    module: 'webforj-foundation',
    input: 'META-INF/resources/webforj/view-transitions.js',
    output: 'META-INF/resources/webforj/view-transitions.min.js'
  },
  {
    module: 'webforj-foundation',
    input: 'static/webforj/icon-badge/icon-badge.js',
    output: 'static/webforj/icon-badge/icon-badge.min.js'
  },
  {
    module: 'webforj-devtools',
    input: 'META-INF/resources/webforj/livereload-client.js',
    output: 'META-INF/resources/webforj/livereload-client.min.js'
  }
];

function moduleResources(module) {
  return path.join(__dirname, '..', module, 'src/main/resources');
}

function minify(module, inputFile, outputFile) {
  const resources = moduleResources(module);
  const inputPath = path.join(resources, inputFile);
  const outputPath = path.join(resources, outputFile);

  if (!fs.existsSync(inputPath)) {
    console.error(`Error: Input file not found: ${inputPath}`);
    process.exit(1);
  }

  const originalSize = fs.statSync(inputPath).size;

  execSync(`npx --yes ${ESBUILD} "${inputPath}" --minify --outfile="${outputPath}"`, {
    stdio: 'pipe'
  });

  const minifiedContent = fs.readFileSync(outputPath, 'utf8');
  fs.writeFileSync(outputPath, BANNER + minifiedContent, 'utf8');

  const minifiedSize = fs.statSync(outputPath).size;
  const reduction = Math.round((1 - minifiedSize / originalSize) * 100);

  console.log(
    `${inputFile}: ${originalSize} -> ${minifiedSize} bytes (${reduction}% reduction)`
  );
}

function minifyHtml(module, inputFile, outputFile) {
  const resources = moduleResources(module);
  const inputPath = path.join(resources, inputFile);
  const outputPath = path.join(resources, outputFile);

  if (!fs.existsSync(inputPath)) {
    console.error(`Error: Input file not found: ${inputPath}`);
    process.exit(1);
  }

  const originalSize = fs.statSync(inputPath).size;

  execSync(
    `npx --yes ${HTML_MINIFIER} --collapse-whitespace --remove-comments` +
      ` --minify-css true --minify-js true -o "${outputPath}" "${inputPath}"`,
    { stdio: 'pipe' }
  );

  const minifiedSize = fs.statSync(outputPath).size;
  const reduction = Math.round((1 - minifiedSize / originalSize) * 100);

  console.log(
    `${inputFile}: ${originalSize} -> ${minifiedSize} bytes (${reduction}% reduction)`
  );
}

function run() {
  console.log('Minifying webforJ resources...\n');

  for (const file of files) {
    minify(file.module, file.input, file.output);
  }

  for (const file of htmlFiles) {
    minifyHtml(file.module, file.input, file.output);
  }

  console.log('\nDone!');
}

run();
