#!/usr/bin/env node

/**
 * Build Docs Index Script
 *
 * Generates docs-index.json for webforJ DevTools.
 *
 * Usage:
 *   node scripts/build-docs-index.js <docs-folder-path>
 *
 * Example:
 *   node scripts/build-docs-index.js /path/to/webforj-documentation/docs/docs/components
 *
 * Output:
 *   - webforj-devtools/src/main/resources/META-INF/resources/docs-index.json
 */

const fs = require('fs');
const path = require('path');

// Configuration
const OUTPUT_DIR = path.join(__dirname, '../webforj-devtools/src/main/resources/META-INF/resources');

// Java source paths to scan for @NodeName annotations
const JAVA_SOURCE_PATHS = [
  path.join(__dirname, '../webforj-foundation/src/main/java'),
  path.join(__dirname, '../webforj-components')
];

/**
 * Recursively scan directory for files with extension
 */
function scanFiles(dir, extension) {
  const files = [];
  if (!fs.existsSync(dir)) return files;

  function scan(currentDir) {
    const entries = fs.readdirSync(currentDir, { withFileTypes: true });
    for (const entry of entries) {
      const fullPath = path.join(currentDir, entry.name);
      if (entry.isDirectory()) {
        if (entry.name === 'test' || entry.name === 'tests') continue;
        scan(fullPath);
      } else if (entry.isFile() && entry.name.endsWith(extension)) {
        files.push(fullPath);
      }
    }
  }

  scan(dir);
  return files;
}

/**
 * Determine the Maven module name from file path
 */
function getModuleFromPath(filePath) {
  // Match webforj-xxx or webforj-components/webforj-xxx, with either path separator
  const match = filePath.match(/webforj-([a-z-]+)[/\\]src[/\\]main[/\\]java/);
  if (match) {
    return match[1];
  }
  return 'foundation';
}

/**
 * Scan Java files for @NodeName annotations to build class -> { dwcTag, module, since } mapping
 */
function scanJavaComponents() {
  const mapping = {}; // className -> { dwcTag, module, since }

  for (const basePath of JAVA_SOURCE_PATHS) {
    const javaFiles = scanFiles(basePath, '.java');

    for (const file of javaFiles) {
      const content = fs.readFileSync(file, 'utf-8');

      // Find @NodeName("xxx") - matches both dwc-* and other tags like google-chart
      const nodeNameMatch = content.match(/@NodeName\("([^"]+)"\)/);
      if (!nodeNameMatch) continue;

      const dwcTag = nodeNameMatch[1];

      // Find package
      const packageMatch = content.match(/^package\s+([\w.]+);/m);
      if (!packageMatch) continue;

      // Find class name
      const classMatch = content.match(/public\s+(?:final\s+)?(?:abstract\s+)?class\s+(\w+)/);
      if (!classMatch) continue;

      // Find @since in javadoc
      const sinceMatch = content.match(/@since\s+([\d.]+)/);
      const since = sinceMatch ? sinceMatch[1] : null;

      const fullClass = `${packageMatch[1]}.${classMatch[1]}`;
      const module = getModuleFromPath(file);
      mapping[fullClass] = { dwcTag, module, since };
    }
  }

  return mapping;
}

/**
 * Parse markdown file and extract metadata
 */
function parseMarkdownFile(filePath, docsBasePath, javaMapping) {
  const content = fs.readFileSync(filePath, 'utf-8');

  // Extract JavadocLink with top='true'
  const javadocLinkMatch = content.match(/<JavadocLink[^>]+top=['"]true['"][^>]*>/);
  if (!javadocLinkMatch) {
    return null;
  }

  const javadocTag = javadocLinkMatch[0];
  const locationMatch = javadocTag.match(/location="([^"]+)"/);
  const typeMatch = javadocTag.match(/type="([^"]+)"/);

  if (!locationMatch) {
    return null;
  }

  const javadocLocation = locationMatch[1];
  const javadocTypeFromDocs = typeMatch ? typeMatch[1] : null;
  const classNameFromDocs = javadocLocation.replace(/\//g, '.');

  // Extract client component tags from docs (dwc-* or other like google-chart)
  const dwcMatches = [];
  const dwcRegex = /<DocChip[^>]+chip=['"]name['"][^>]+label="([^"]+)"/g;
  let match;
  while ((match = dwcRegex.exec(content)) !== null) {
    dwcMatches.push(match[1]);
  }

  // Extract 'since' version
  const sinceMatch = content.match(/<DocChip[^>]+chip=['"]since['"][^>]+label=['"]([^'"]+)['"]/);
  const since = sinceMatch ? sinceMatch[1] : null;

  // Extract title and description from frontmatter
  let title = null;
  let description = '';
  const frontmatterMatch = content.match(/^---\s*\n([\s\S]*?)\n---/);
  if (frontmatterMatch) {
    const titleMatch = frontmatterMatch[1].match(/title:\s*(.+)/);
    if (titleMatch) {
      title = titleMatch[1].trim().replace(/^["']|["']$/g, '');
    }
    const descriptionMatch = frontmatterMatch[1].match(/description:\s*(.+)/);
    if (descriptionMatch) {
      description = descriptionMatch[1].trim().replace(/^["']|["']$/g, '');
    }
  }
  if (!title) {
    const headingMatch = content.match(/^#\s+(.+)$/m);
    if (headingMatch) {
      title = headingMatch[1].trim();
    }
  }
  if (!title) {
    title = path.basename(filePath, '.md')
      .replace(/[-_]/g, ' ')
      .replace(/\b\w/g, c => c.toUpperCase());
  }

  // Compute docs URL
  const relativePath = path.relative(docsBasePath, filePath).replace(/\\/g, '/');
  const docsUrl = `https://docs.webforj.com/docs/components/${relativePath.replace(/\.md$/, '')}`;

  // Find the actual Java class from @NodeName mapping by DWC tag
  const primaryDwcTag = dwcMatches[0] || null;
  let actualClassName = null;
  let javaInfo = null;
  if (primaryDwcTag) {
    for (const [className, info] of Object.entries(javaMapping)) {
      if (info.dwcTag === primaryDwcTag) {
        actualClassName = className;
        javaInfo = info;
        break;
      }
    }
  }

  // Build javadoc URL: markdown type first, then Java source module, then foundation
  const javadocModule = javadocTypeFromDocs || javaInfo?.module || 'foundation';
  const javadocUrl = `https://javadoc.io/doc/com.webforj/webforj-${javadocModule}/latest/${javadocLocation}.html`;

  return {
    classNameFromDocs,
    actualClassName,
    dwcTags: dwcMatches,
    entry: {
      title,
      since: since || javaInfo?.since || undefined,
      javadoc: javadocUrl,
      docs: docsUrl,
      clientComponent: primaryDwcTag || undefined,
      content: description
    }
  };
}

/**
 * Main function
 */
async function main() {
  const docsPath = process.argv[2];

  if (!docsPath) {
    console.error('Usage: node build-docs-index.js <docs-folder-path>');
    console.error('');
    console.error('Example:');
    console.error('  node build-docs-index.js /path/to/webforj-documentation/docs/docs/components');
    process.exit(1);
  }

  if (!fs.existsSync(docsPath)) {
    console.error(`Error: Directory not found: ${docsPath}`);
    process.exit(1);
  }

  console.log('Building docs index...');
  console.log(`  Docs path: ${docsPath}`);
  console.log(`  Output dir: ${OUTPUT_DIR}`);
  console.log('');

  // Ensure output directory exists
  fs.mkdirSync(OUTPUT_DIR, { recursive: true });

  // Scan Java files for @NodeName mappings
  console.log('Scanning Java sources for @NodeName...');
  const javaMapping = scanJavaComponents();
  console.log(`  Found ${Object.keys(javaMapping).length} components with @NodeName`);

  // Scan markdown files
  console.log('Scanning markdown files...');
  const mdFiles = scanFiles(docsPath, '.md');
  console.log(`  Found ${mdFiles.length} markdown files`);

  // Parse each file and build index
  console.log('Parsing documentation...');
  const docsIndex = {};

  for (const file of mdFiles) {
    const result = parseMarkdownFile(file, docsPath, javaMapping);
    if (!result) continue;

    // Use actual Java class name if available, otherwise use docs path
    const className = result.actualClassName || result.classNameFromDocs;

    docsIndex[className] = result.entry;
    console.log(`  + ${className}`);
  }

  // Add Java components that have @NodeName but no docs entry
  console.log('Adding components from Java sources...');
  for (const [className, info] of Object.entries(javaMapping)) {
    if (docsIndex[className]) continue; // Already have docs for this class

    const title = className.split('.').pop();
    const javadocPath = className.replace(/\./g, '/');
    const javadocUrl = `https://javadoc.io/doc/com.webforj/webforj-${info.module}/latest/${javadocPath}.html`;

    docsIndex[className] = {
      title,
      since: info.since || undefined,
      javadoc: javadocUrl,
      clientComponent: info.dwcTag
    };

    console.log(`  + ${className} (from Java, module: ${info.module})`);
  }

  // Sort by class name and write output file
  console.log('');
  console.log('Writing output file...');

  const sortedDocsIndex = Object.keys(docsIndex)
    .sort()
    .reduce((acc, key) => {
      acc[key] = docsIndex[key];
      return acc;
    }, {});

  const docsIndexPath = path.join(OUTPUT_DIR, 'docs-index.json');
  fs.writeFileSync(docsIndexPath, JSON.stringify(sortedDocsIndex, null, 2));
  console.log(`  ${docsIndexPath}`);

  console.log('');
  console.log('Done!');
  console.log(`  Total entries: ${Object.keys(docsIndex).length}`);
}

main().catch(e => {
  console.error('Error:', e.message);
  process.exit(1);
});
