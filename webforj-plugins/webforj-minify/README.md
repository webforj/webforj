# webforJ Minify Plugin

Build-time asset minification system for webforJ applications that automatically discovers and minifies CSS and JavaScript resources referenced via webforJ annotations.

## Features

- 🚀 **Zero Configuration** - Works out of the box for 90% of use cases
- 🔍 **Automatic Discovery** - Scans webforJ annotations at compile time
- ⚡ **Parallel Processing** - Minifies multiple files concurrently for large projects
- 🔌 **Pluggable Architecture** - Extensible via Java SPI
- 🛡️ **Graceful Error Handling** - Minification errors don't fail builds
- 📊 **Performance Metrics** - Detailed logging with size reduction statistics
- 🔒 **Security Validated** - Directory traversal protection

## Requirements

- Java 17 or higher
- Maven 3.6+ or Gradle 7.6+
- webforJ framework (any version with annotation support)

## Quick Start

### Maven Setup

Add the plugin to your `pom.xml`:

```xml
<build>
  <plugins>
    <!-- 1. Annotation Processor (discovers assets during compilation) -->
    <!-- NOTE: This configuration is required because annotation processing happens
         during compilation, before the minify plugin executes. Maven/Gradle cannot
         automatically discover annotation processors from plugin dependencies. -->
    <plugin>
      <groupId>org.apache.maven.plugins</groupId>
      <artifactId>maven-compiler-plugin</artifactId>
      <configuration>
        <annotationProcessorPaths>
          <path>
            <groupId>com.webforj</groupId>
            <artifactId>webforj-minify-common</artifactId>
            <version>25.10-SNAPSHOT</version>
          </path>
        </annotationProcessorPaths>
      </configuration>
    </plugin>

    <!-- 2. Minify Plugin (minifies discovered assets after compilation) -->
    <plugin>
      <groupId>com.webforj</groupId>
      <artifactId>webforj-minify-maven-plugin</artifactId>
      <version>25.10-SNAPSHOT</version>
      <executions>
        <execution>
          <goals>
            <goal>minify</goal>
          </goals>
        </execution>
      </executions>
      <dependencies>
        <!-- CSS minification -->
        <dependency>
          <groupId>com.webforj</groupId>
          <artifactId>webforj-minify-css</artifactId>
          <version>25.10-SNAPSHOT</version>
        </dependency>
        <!-- JavaScript minification -->
        <dependency>
          <groupId>com.webforj</groupId>
          <artifactId>webforj-minify-js</artifactId>
          <version>25.10-SNAPSHOT</version>
        </dependency>
      </dependencies>
    </plugin>
  </plugins>
</build>
```

### Gradle Setup

Add the plugin to your `build.gradle`:

```groovy
plugins {
    id 'java'
    id 'com.webforj.minify' version '25.10-SNAPSHOT'
}

repositories {
    mavenCentral()
    mavenLocal()
}

dependencies {
    // Annotation processor for discovering assets
    annotationProcessor 'com.webforj:webforj-minify-common:25.10-SNAPSHOT'
}

// Add minifiers as plugin dependencies
buildscript {
    dependencies {
        classpath 'com.webforj:webforj-minify-css:25.10-SNAPSHOT'
        classpath 'com.webforj:webforj-minify-js:25.10-SNAPSHOT'
    }
}

// Optional: Configure the plugin
webforjMinify {
    skip = false  // Set to true to disable minification
}
```

**For local development** (using SNAPSHOT versions), add to `settings.gradle`:

```groovy
pluginManagement {
    repositories {
        mavenLocal()
        gradlePluginPortal()
    }
}
```

### Usage Example

```java
package com.example;

import com.webforj.annotation.StyleSheet;
import com.webforj.annotation.JavaScript;

@StyleSheet("webserver://css/app.css")
@JavaScript("ws://js/app.js")
public class MyApp extends App {
  // Your application code
}
```

**Build Output:**
```
[INFO] Starting webforJ asset minification...
[INFO] Discovered 2 minifier implementation(s) via SPI
[INFO] Processing manifest: target/classes/META-INF/webforj-resources.json
[INFO] Found 2 asset(s) in manifest
[INFO] Minified app.css: 1200 → 850 bytes (29.2% reduction) in 45 ms
[INFO] Minified app.js: 3400 → 1800 bytes (47.1% reduction) in 280 ms
[INFO] Minification complete. Processed 2 file(s) in 335 ms
```

## How It Works

### 1. Compile-Time Discovery (Annotation Processor)

The annotation processor scans your source code during compilation for webforJ asset annotations:
- `@StyleSheet`
- `@JavaScript`
- `@InlineStyleSheet`
- `@InlineJavaScript`

It generates a manifest file at `META-INF/webforj-resources.json`:

```json
{
  "version": "1.0",
  "generatedAt": "2025-10-17T12:00:00Z",
  "assets": [
    {
      "url": "webserver://css/app.css",
      "type": "StyleSheet",
      "discoveredIn": "com.example.MyApp"
    }
  ]
}
```

### 2. Build-Time Minification (Maven/Gradle Plugin)

The plugin runs in the `process-classes` phase (after compilation, before packaging):

1. Loads minifier implementations via Java SPI
2. Reads the manifest file
3. Resolves URLs to filesystem paths
4. Minifies assets in parallel (for >10 files)
5. Writes minified content back to original files

### 3. URL Protocol Resolution

The plugin understands webforJ URL protocols:

| Protocol | Resolves To | Example |
|----------|-------------|---------|
| `webserver://` or `ws://` | `src/main/resources/static/` | `ws://css/app.css` → `static/css/app.css` |
| `context://` | `src/main/resources/` | `context://styles/app.css` → `styles/app.css` |
| No protocol | `src/main/resources/static/` | `css/app.css` → `static/css/app.css` |

## Configuration

### Skip Minification

**Maven:**
```bash
mvn package -Dwebforj.minify.skip=true

# Or in pom.xml
<plugin>
  <groupId>com.webforj</groupId>
  <artifactId>webforj-minify-maven-plugin</artifactId>
  <configuration>
    <skip>true</skip>
  </configuration>
</plugin>
```

**Gradle:**
```groovy
// In build.gradle
webforjMinify {
    skip = true
}

// Or via command line
gradle build -Pwebforj.minify.skip=true
```

### Additional Files (Manual Configuration)

Create `src/main/resources/META-INF/webforj-minify.txt`:

```
# Glob patterns for additional files
**/*.css
**/*.js

# Exclusions (prefix with !)
!**/*.min.css
!**/*.min.js
```

### Parallel Processing Threshold

By default, the plugin uses parallel streams for >10 files. This is automatic and requires no configuration.

## Architecture

### Module Structure

```
webforj-minify/
├── webforj-minify-common/       # Core interfaces + annotation processor
│   ├── AssetMinifier            # SPI interface for minifiers
│   ├── MinifierRegistry         # Thread-safe minifier registry
│   ├── ResourceResolver         # URL protocol resolver
│   ├── MinificationException    # Exception type
│   ├── AssetAnnotationProcessor # Generates manifest at compile time
│   └── META-INF/services/javax.annotation.processing.Processor
├── webforj-minify-css/          # CSS minifier module
│   ├── PhCssMinifier            # CSS minification (ph-css 8.0.0)
│   └── META-INF/services/com.webforj.minify.common.AssetMinifier
├── webforj-minify-js/           # JavaScript minifier module
│   ├── ClosureJsMinifier        # JS minification (Closure Compiler v20230802)
│   └── META-INF/services/com.webforj.minify.common.AssetMinifier
├── webforj-minify-maven-plugin/ # Maven plugin (depends only on common)
│   └── MinifyMojo               # Executes minification
└── webforj-minify-gradle-plugin/# Gradle plugin (depends only on common)
    └── MinifyTask               # Gradle task implementation
```

### Extensibility

Create custom minifiers by implementing the `AssetMinifier` interface:

```java
package com.example;

import com.webforj.minify.common.AssetMinifier;
import com.webforj.minify.common.MinificationException;
import java.nio.file.Path;
import java.util.Set;

public class SassMinifier implements AssetMinifier {

  @Override
  public String minify(String content, Path sourceFile) throws MinificationException {
    try {
      // Your minification logic here
      return compileSass(content);
    } catch (Exception e) {
      throw new MinificationException("Failed to minify " + sourceFile, e);
    }
  }

  @Override
  public Set<String> getSupportedExtensions() {
    return Set.of("scss", "sass");
  }

  @Override
  public boolean shouldMinify(Path filePath) {
    // Skip already minified files
    String fileName = filePath.getFileName().toString().toLowerCase();
    return !fileName.endsWith(".min.scss") && !fileName.endsWith(".min.sass");
  }
}
```

Register via SPI by creating `META-INF/services/com.webforj.minify.common.AssetMinifier`:

```
com.example.SassMinifier
```

Then include it in the plugin dependencies (Maven):

```xml
<plugin>
  <groupId>com.webforj</groupId>
  <artifactId>webforj-minify-maven-plugin</artifactId>
  <dependencies>
    <!-- Your custom minifier -->
    <dependency>
      <groupId>com.example</groupId>
      <artifactId>my-custom-minifier</artifactId>
      <version>1.0.0</version>
    </dependency>
    <!-- Standard minifiers (optional) -->
    <dependency>
      <groupId>com.webforj</groupId>
      <artifactId>webforj-minify-css</artifactId>
      <version>25.10-SNAPSHOT</version>
    </dependency>
  </dependencies>
</plugin>
```

Or for Gradle:

```groovy
buildscript {
    dependencies {
        classpath 'com.example:my-custom-minifier:1.0.0'
        classpath 'com.webforj:webforj-minify-css:25.10-SNAPSHOT'
    }
}
```

## Default Minifiers

### CSS Minifier (ph-css 8.0.0)

- Removes comments
- Removes unnecessary whitespace
- Optimizes property values
- Preserves semantic correctness
- Handles media queries and complex selectors

**Features:**
- ✅ CSS3 support
- ✅ @media queries
- ✅ Vendor prefixes
- ✅ Graceful error handling (returns original on parse failure)

### JavaScript Minifier (Closure Compiler v20230802)

- Removes comments and whitespace
- Renames local variables
- Dead code elimination
- ES6+ syntax support

**Configuration:**
- Compilation level: `SIMPLE_OPTIMIZATIONS`
- Language in: `ECMASCRIPT_2015` (ES6)
- Language out: `ECMASCRIPT_2015` (ES6)
- Preserves function names (for debugging)

**Features:**
- ✅ ES6 arrow functions, classes, template literals
- ✅ const/let support
- ✅ Graceful error handling (returns original on compilation errors)

## Performance

### Benchmarks

Tested on MacBook M2, Java 17.0.16 (Temurin):

| File Count | Size | Processing Mode | Time |
|------------|------|----------------|------|
| 2 files | 1.3 KB | Sequential | 355 ms |
| 10 files | 12 KB | Sequential | ~1.2 s |
| 25 files | 50 KB | Parallel | ~2.5 s |
| 100 files | 250 KB | Parallel | ~8 s |

### Optimization Tips

1. **Already Minified Files**: The default CSS and JS minifiers automatically skip files matching `*.min.css` or `*.min.js` via their `shouldMinify()` method
2. **Parallel Processing**: Automatically enabled for >10 files
3. **Incremental Builds**: Only changed files are processed (Maven incremental compilation)
4. **CI/CD**: Enable minification only for production builds with profiles
5. **Custom Skip Logic**: Implement custom `shouldMinify()` logic in your minifiers to skip files based on any criteria

## Troubleshooting

### No minifiers registered via SPI

**Error:**
```
[WARN] No minifiers registered via SPI. Skipping minification.
[WARN] Ensure minifier modules are on the classpath.
```

**Solution:** Add minifier module dependencies to the plugin configuration:
- For CSS: Add `webforj-minify-css` as a plugin dependency
- For JavaScript: Add `webforj-minify-js` as a plugin dependency

See Quick Start section for complete configuration examples.

### Manifest file not found

**Issue:** Plugin can't find `META-INF/webforj-resources.json`

**Solutions:**
1. Ensure annotation processor is configured correctly:
   - Maven: Add `webforj-minify-common` to `annotationProcessorPaths` in `maven-compiler-plugin`
   - Gradle: Add `annotationProcessor 'com.webforj:webforj-minify-common:VERSION'` to dependencies
2. Check that webforJ annotations are present in source code
3. Verify `target/classes/META-INF/webforj-resources.json` exists after compilation
4. Ensure `<proc>none</proc>` is NOT set in the compiler plugin (it disables annotation processing)

### File not found errors

**Error:**
```
[WARN] File not found: /path/to/static/css/app.css
```

**Solutions:**
1. Verify file exists at correct path under `src/main/resources/`
2. Check URL protocol matches directory structure
3. Ensure file wasn't excluded by `.gitignore`

### Minification errors

**Behavior:** Plugin warns but continues (doesn't fail build)

```
[WARN] Error minifying file app.css: Parse error at line 42
```

**This is intentional!** Minification errors return original content to prevent build failures.

**To fix:**
1. Validate CSS/JS syntax with a linter
2. Check for unsupported features
3. Review error message for specific line/column

### Security violations

**Error:**
```
[WARN] Security violation for URL '../../../etc/passwd': Path traversal detected
```

**Cause:** URL attempts to escape resources directory

**Solution:** Use valid webforJ protocols and paths

## Development

### Building from Source

```bash
cd webforj-minify
mvn clean install
```

### Running Tests

```bash
# All tests
mvn clean verify

# Specific module tests
cd webforj-minify-common
mvn test

cd ../webforj-minify-css
mvn test

cd ../webforj-minify-js
mvn test
```

### Test Coverage

- **35 unit tests** covering all modules (21 common, 6 CSS, 8 JavaScript)
- **Code coverage** tracked via JaCoCo
- **Security tests** for directory traversal protection
- **Error handling tests** for graceful minification failures

## Roadmap

- [x] Gradle plugin implementation
- [ ] Source map generation
- [ ] Configurable compilation levels
- [ ] CSS/JS validation before minification
- [ ] Watch mode for development

## License

Part of the webforJ project. See parent repository for license details.

## Contributing

Contributions welcome! Please follow webforJ contribution guidelines:

1. Run Checkstyle before committing
2. Ensure all tests pass
3. Follow Conventional Commits format
4. Add unit tests for new features

## Support

- **Issues**: https://github.com/webforj/webforj/issues
- **Documentation**: https://docs.webforj.com
- **Community**: https://discord.gg/webforj
