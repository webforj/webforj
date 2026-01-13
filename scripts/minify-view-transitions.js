/**
 * Minifies view-transitions.css using esbuild
 *
 * Usage: node scripts/minify-view-transitions.js
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

const RESOURCES_DIR = path.join(
  __dirname,
  '../webforj-foundation/src/main/resources/META-INF/resources/webforj'
);

const files = [
  { input: 'view-transitions.css', output: 'view-transitions.min.css' },
  { input: 'view-transitions.js', output: 'view-transitions.min.js' }
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
  const inputPath = path.join(RESOURCES_DIR, inputFile);
  const outputPath = path.join(RESOURCES_DIR, outputFile);

  if (!fs.existsSync(inputPath)) {
    console.error(`Error: Input file not found: ${inputPath}`);
    process.exit(1);
  }

  const originalSize = fs.statSync(inputPath).size;

  execSync(`esbuild "${inputPath}" --minify --outfile="${outputPath}"`, {
    stdio: 'pipe'
  });

  // Prepend banner
  const minifiedContent = fs.readFileSync(outputPath, 'utf8');
  fs.writeFileSync(outputPath, BANNER + minifiedContent, 'utf8');

  const minifiedSize = fs.statSync(outputPath).size;
  const reduction = Math.round((1 - minifiedSize / originalSize) * 100);

  console.log(
    `${inputFile}: ${originalSize} -> ${minifiedSize} bytes (${reduction}% reduction)`
  );
}

function run() {
  console.log('Minifying view transitions...\n');

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
