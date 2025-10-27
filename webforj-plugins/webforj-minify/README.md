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
            <artifactId>webforj-minify-foundation</artifactId>
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
    annotationProcessor 'com.webforj:webforj-minify-foundation:25.10-SNAPSHOT'
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

@StyleSheet("ws://css/app.css")
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
      "url": "ws://css/app.css",
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
| `ws://` | `src/main/resources/static/` | `ws://css/app.css` → `static/css/app.css` |
| `context://` | `src/main/resources/` | `context://styles/app.css` → `styles/app.css` |

**Note:** URLs without a protocol (e.g., `css/app.css`) are NOT supported for minification because webforJ passes them through unchanged to the browser, which resolves them as relative URLs. These cannot be reliably mapped to filesystem paths. Use `ws://` or `context://` for assets that need minification.

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

## Frequently Asked Questions

### Why is the annotation processor configuration required? Can it be avoided?

**Short Answer:** No, the annotation processor configuration cannot be avoided or automated. This is a fundamental requirement of Maven/Gradle build lifecycles and is standard practice for ALL Java annotation processors.

**Detailed Explanation:**

You may have noticed that the setup requires explicit configuration of the annotation processor in the `maven-compiler-plugin` (using `<annotationProcessorPaths>`) or Gradle (using `annotationProcessor` dependency). **This configuration is mandatory and cannot be hidden, automated, or bundled with the minify plugin.**

**Why This Cannot Be Avoided:**

1. **Build Lifecycle Phases (Fundamental Constraint):**
   - Annotation processing happens during the **compile phase** (when javac runs)
   - The minify plugin executes during the **process-classes phase** (after compilation completes)
   - By the time the minify plugin executes, compilation is already finished
   - Plugins cannot retroactively modify the compiler plugin's configuration for past phases

2. **Maven 3+ Security Model (Deliberate Design):**
   - Maven 3.0+ introduced a security model that prevents automatic classpath scanning for annotation processors
   - This prevents malicious code execution during compilation
   - Annotation processors must be explicitly declared in `annotationProcessorPaths` or as compile dependencies
   - This is a **deliberate design decision** by the Maven team, not a limitation

3. **Gradle Annotation Processing Model:**
   - Gradle requires explicit declaration of annotation processors via the `annotationProcessor` configuration
   - This separates compile-time tooling from runtime dependencies
   - Gradle does not support automatic discovery from plugin classpaths

**This Is Standard Practice:**

Every major Java annotation processor requires the same configuration:
- **Lombok:** Requires `annotationProcessorPaths` or `annotationProcessor` dependency
- **MapStruct:** Requires `annotationProcessorPaths` or `annotationProcessor` dependency
- **Dagger:** Requires `annotationProcessorPaths` or `annotationProcessor` dependency
- **Google Auto:** Requires `annotationProcessorPaths` or `annotationProcessor` dependency

The webforJ minify plugin follows the same industry-standard approach.

**Alternative Approach (Not Recommended):**

If you strongly prefer to avoid `annotationProcessorPaths`, you can add `webforj-minify-foundation` as a regular compile dependency with `provided` scope:

```xml
<dependencies>
  <dependency>
    <groupId>com.webforj</groupId>
    <artifactId>webforj-minify-foundation</artifactId>
    <version>25.10-SNAPSHOT</version>
    <scope>provided</scope>
  </dependency>
</dependencies>

<!-- maven-compiler-plugin will auto-discover from compile classpath -->
<plugin>
  <groupId>org.apache.maven.plugins</groupId>
  <artifactId>maven-compiler-plugin</artifactId>
  <!-- No annotationProcessorPaths needed with above approach -->
</plugin>
```

**However**, this approach:
- ✅ Simplifies configuration (no `annotationProcessorPaths` needed)
- ❌ Adds an unnecessary compile dependency to your project
- ❌ Increases your project's dependency tree complexity
- ❌ Goes against Maven best practices (separate build-time from compile-time dependencies)

**Recommendation:** Use `annotationProcessorPaths` (as shown in Quick Start). This is the correct, industry-standard approach that keeps your dependencies clean and minimal.

## Architecture

### Module Structure

```
webforj-minify/
├── webforj-minify-foundation/   # Core interfaces + annotation processor
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
│   ├── ClosureJsMinifier        # JS minification (Closure Compiler v20250820)
│   └── META-INF/services/com.webforj.minify.common.AssetMinifier
├── webforj-minify-maven-plugin/ # Maven plugin (depends only on foundation)
│   └── MinifyMojo               # Executes minification
└── webforj-minify-gradle-plugin/# Gradle plugin (depends only on foundation)
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

The plugin provides two optional minifier implementations as separate modules. Users include only the minifiers they need as plugin dependencies.

### CSS Minifier (`webforj-minify-css`)

**Implementation:** `PhCssMinifier` using ph-css 8.0.0

**Features:**
- ✅ Removes comments and unnecessary whitespace
- ✅ Optimizes property values
- ✅ Preserves semantic correctness
- ✅ CSS3 support with @media queries and vendor prefixes
- ✅ Automatically skips `*.min.css` files via `shouldMinify()`
- ✅ Graceful error handling (returns original on parse failure)

**Module:** Add as plugin dependency
```xml
<dependency>
  <groupId>com.webforj</groupId>
  <artifactId>webforj-minify-css</artifactId>
  <version>25.10-SNAPSHOT</version>
</dependency>
```

### JavaScript Minifier (`webforj-minify-js`)

**Implementation:** `ClosureJsMinifier` using Google Closure Compiler v20250820

**Configuration:**
- Compilation level: `SIMPLE_OPTIMIZATIONS` (no symbol renaming)
- Language in: `ECMASCRIPT_NEXT` (accepts modern ES6+ syntax)
- Language out: `ECMASCRIPT5` (broad browser compatibility)

**Features:**
- ✅ Removes comments and whitespace
- ✅ Renames local variables and optimizes code
- ✅ Dead code elimination
- ✅ ES6+ syntax support (arrow functions, classes, template literals, const/let)
- ✅ Automatically skips `*.min.js` files via `shouldMinify()`
- ✅ Graceful error handling (returns original on compilation errors)

**Module:** Add as plugin dependency
```xml
<dependency>
  <groupId>com.webforj</groupId>
  <artifactId>webforj-minify-js</artifactId>
  <version>25.10-SNAPSHOT</version>
</dependency>
```

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
   - Maven: Add `webforj-minify-foundation` to `annotationProcessorPaths` in `maven-compiler-plugin`
   - Gradle: Add `annotationProcessor 'com.webforj:webforj-minify-foundation:VERSION'` to dependencies
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
