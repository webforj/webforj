/**
 * Minifies webforJ client-side resources using esbuild.
 *
 * Usage: node scripts/minify.js
 *
 * Requires esbuild to be installed: npm install -g esbuild
 * Or download standalone binary from: https://github.com/evanw/esbuild/releases
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

const FOUNDATION_RESOURCES = path.join(
  __dirname,
  '../webforj-foundation/src/main/resources'
);

const files = [
  {
    input: 'META-INF/resources/webforj/view-transitions.css',
    output: 'META-INF/resources/webforj/view-transitions.min.css'
  },
  {
    input: 'META-INF/resources/webforj/view-transitions.js',
    output: 'META-INF/resources/webforj/view-transitions.min.js'
  },
  {
    input: 'static/webforj/icon-badge/icon-badge.js',
    output: 'static/webforj/icon-badge/icon-badge.min.js'
  }
];

function checkEsbuild() {
  try {
    execSync('esbuild --version', { stdio: 'pipe' });
    return true;
  } catch {
    return false;
  }
}

function minify(inputFile, outputFile) {
  const inputPath = path.join(FOUNDATION_RESOURCES, inputFile);
  const outputPath = path.join(FOUNDATION_RESOURCES, outputFile);

  if (!fs.existsSync(inputPath)) {
    console.error(`Error: Input file not found: ${inputPath}`);
    process.exit(1);
  }

  const originalSize = fs.statSync(inputPath).size;

  execSync(`esbuild "${inputPath}" --minify --outfile="${outputPath}"`, {
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

function run() {
  console.log('Minifying webforJ resources...\n');

  if (!checkEsbuild()) {
    console.error('Error: esbuild not found.');
    console.error('Install with: npm install -g esbuild');
    console.error('Or download from: https://github.com/evanw/esbuild/releases');
    process.exit(1);
  }

  for (const file of files) {
    minify(file.input, file.output);
  }

  console.log('\nDone!');
}

run();
