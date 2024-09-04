/**
 * Script to generate a Java enum class from the Feather icons repository.
 *
 * Usage:
 *  node scripts/feather-icons.js [version]
 *
 * If the version is not provided as an argument, the script will prompt the user to enter it.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');
const readline = require('readline');

const askVersionNumber = () => {
  return new Promise((resolve) => {
    const rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout
    });

    rl.question('Please enter the version number: ', (version) => {
      rl.close();
      resolve(version);
    });
  });
};

const cloneRepo = (version, targetDir) => {
  const repoUrl = 'https://github.com/feathericons/feather.git';
  const cloneCommand = `git clone --branch ${version} --depth 1 ${repoUrl} ${targetDir}`;

  try {
    execSync(cloneCommand, { stdio: 'inherit' });
    console.log('Repository cloned successfully.');
  } catch (error) {
    console.error('Error cloning repository:', error.message);
    process.exit(1);
  }
};

const generateEnum = (icons, version) => {
  let enumContent = `package com.webforj.component.icons;

import java.util.Locale;

/**
 * Enumeration of all Feather icons ${version}.
 *
 * @see <a href="https://feathericons.com/">Feather Icons</a>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public enum FeatherIcon implements IconFactory {
  // @formatter:off
`;

  icons.forEach((icon, index) => {
    const iconName = icon.toUpperCase().replace(/-/g, '_').replace(/\.SVG$/i, '');
    const suffix = index === icons.length - 1 ? ';' : ',';
    enumContent += `  ${iconName}${suffix}\n`;
  });

  enumContent += `  // @formatter:on

  /**
   * {@inheritDoc}
   */
  @Override
  public Icon create() {
    return new Icon(String.valueOf(this), getPool());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return name().toLowerCase(Locale.ENGLISH).replace('-', '_');
  }
}
`;
  return enumContent;
};

const scanSvgFiles = (dir, prefix = '') => {
  return fs.readdirSync(dir)
    .filter(file => path.extname(file).toLowerCase() === '.svg')
    .map(file => prefix + file);
};

const cleanUp = (dir) => {
  if (fs.existsSync(dir)) {
    fs.rmSync(dir, { recursive: true, force: true });
    console.log(`Temporary directory ${dir} cleaned up.`);
  }
};

const run = async () => {
  let version = process.argv[2];

  if (!version) {
    version = await askVersionNumber();
  }

  const tempDir = path.join(__dirname, '.tmp', 'feather-icons');
  const iconsPath = path.join(tempDir, 'icons');

  if (!fs.existsSync(tempDir)) {
    fs.mkdirSync(tempDir, { recursive: true });
  }

  cleanUp(tempDir);
  cloneRepo(version, tempDir);

  const allIcons = [
    ...scanSvgFiles(iconsPath),
  ];

  const enumContent = generateEnum(allIcons, version);
  const outputFilePath = path.join(__dirname, '../webforj-components/webforj-icons/src/main/java/com/webforj/component/icons/FeatherIcon.java');
  fs.writeFileSync(outputFilePath, enumContent, 'utf8');

  console.log(`Java enum file created at: ${outputFilePath}`);

  cleanUp(tempDir);
};

run();

