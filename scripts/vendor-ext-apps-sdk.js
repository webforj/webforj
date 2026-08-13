/**
 * Updates the vendored MCP Apps browser SDK to a published npm version.
 *
 * Usage: node scripts/vendor-ext-apps-sdk.js [version]
 *
 * Without a version the latest published release is taken.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
const { execSync } = require('child_process');
const crypto = require('crypto');
const fs = require('fs');
const os = require('os');
const path = require('path');

const PACKAGE = '@modelcontextprotocol/ext-apps';
const REGISTRY = 'https://registry.npmjs.org';
const BUNDLE_IN_TARBALL = 'package/dist/src/app-with-deps.js';
const LICENSE_IN_TARBALL = 'package/LICENSE';
const GLOBAL_NAME = 'McpExtApps';

const ESBUILD = 'esbuild@0.27.1';

const RESOURCES = path.join(
  __dirname,
  '..',
  'webforj-mcp-apps/src/main/resources/META-INF/mcp'
);
const RESOURCE_CLASS = path.join(
  __dirname,
  '..',
  'webforj-mcp-apps/src/main/java/com/webforj/mcp/McpAppResource.java'
);

async function resolveRelease(version) {
  const response = await fetch(`${REGISTRY}/${PACKAGE}/${version || 'latest'}`);

  if (!response.ok) {
    console.error(
      `Error: ${PACKAGE}@${version || 'latest'} not found on the registry (HTTP ${response.status})`
    );
    process.exit(1);
  }

  const metadata = await response.json();
  return {
    version: metadata.version,
    tarball: metadata.dist.tarball,
    integrity: metadata.dist.integrity
  };
}

async function downloadVerified(release, target) {
  const response = await fetch(release.tarball);

  if (!response.ok) {
    console.error(`Error: tarball download failed (HTTP ${response.status})`);
    process.exit(1);
  }

  const bytes = Buffer.from(await response.arrayBuffer());
  const [algorithm, expected] = release.integrity.split('-', 2);
  const actual = crypto.createHash(algorithm).update(bytes).digest('base64');

  if (actual !== expected) {
    console.error('Error: the tarball does not match the registry integrity hash.');
    console.error(`  expected ${release.integrity}`);
    console.error(`  received ${algorithm}-${actual}`);
    process.exit(1);
  }

  fs.writeFileSync(target, bytes);
}

function toClassicScript(bundle, version, workDirectory) {
  const converted = path.join(workDirectory, 'app-with-deps.iife.js');
  const banner = `/*! ${PACKAGE} ${version} | see ext-apps-${version}.LICENSE */`;

  // The conversion runs from the work directory on relative paths, so the source reference
  // esbuild writes into the output names the tarball entry and never a temporary location,
  // which keeps regeneration byte identical across runs and machines.
  execSync(
    `npx --yes ${ESBUILD} "${path.relative(workDirectory, bundle)}" --bundle --minify --format=iife`
      + ` --global-name=${GLOBAL_NAME} --banner:js="${banner}"`
      + ` --outfile="${path.relative(workDirectory, converted)}"`,
    { stdio: 'pipe', cwd: workDirectory }
  );

  const content = fs.readFileSync(converted, 'utf8');
  if (!content.includes(`var ${GLOBAL_NAME}`)) {
    console.error(`Error: the converted bundle does not define the ${GLOBAL_NAME} global.`);
    process.exit(1);
  }

  return converted;
}

function extract(tarball, entry, workDirectory) {
  execSync(`tar -xzf "${tarball}" -C "${workDirectory}" "${entry}"`, { stdio: 'pipe' });

  const extracted = path.join(workDirectory, entry);
  if (!fs.existsSync(extracted)) {
    console.error(`Error: ${entry} is missing from the tarball.`);
    process.exit(1);
  }

  return extracted;
}

function replaceVendoredFiles(version, bundle, license) {
  for (const stale of fs.readdirSync(RESOURCES)) {
    if (/^ext-apps-.*\.(js|LICENSE)$/.test(stale)) {
      fs.rmSync(path.join(RESOURCES, stale));
      console.log(`Removed ${stale}`);
    }
  }

  const bundleName = `ext-apps-${version}.js`;
  const licenseName = `ext-apps-${version}.LICENSE`;
  fs.copyFileSync(bundle, path.join(RESOURCES, bundleName));
  fs.copyFileSync(license, path.join(RESOURCES, licenseName));

  const sha256 = crypto
    .createHash('sha256')
    .update(fs.readFileSync(bundle))
    .digest('hex');
  console.log(`Added ${bundleName} (${fs.statSync(bundle).size} bytes, sha256 ${sha256})`);
  console.log(`Added ${licenseName}`);

  return bundleName;
}

function updateResourceClass(bundleName) {
  const source = fs.readFileSync(RESOURCE_CLASS, 'utf8');
  const updated = source.replace(/ext-apps-[^"]+\.js/, bundleName);

  if (updated === source && !source.includes(bundleName)) {
    console.error(`Error: no ext-apps reference found in ${RESOURCE_CLASS}`);
    process.exit(1);
  }

  fs.writeFileSync(RESOURCE_CLASS, updated, 'utf8');
  console.log(`Updated ${path.basename(RESOURCE_CLASS)} to ${bundleName}`);
}

async function run() {
  const release = await resolveRelease(process.argv[2]);
  console.log(`Updating ${PACKAGE} to ${release.version}...\n`);

  const workDirectory = fs.mkdtempSync(path.join(os.tmpdir(), 'ext-apps-'));

  try {
    const tarball = path.join(workDirectory, 'package.tgz');
    await downloadVerified(release, tarball);

    const bundle = extract(tarball, BUNDLE_IN_TARBALL, workDirectory);
    const license = extract(tarball, LICENSE_IN_TARBALL, workDirectory);
    const classicScript = toClassicScript(bundle, release.version, workDirectory);

    const bundleName = replaceVendoredFiles(release.version, classicScript, license);
    updateResourceClass(bundleName);
  } finally {
    fs.rmSync(workDirectory, { recursive: true, force: true });
  }

  console.log('\nDone! Rebuild webforj-mcp-apps and run its tests to verify the page.');
}

run();
