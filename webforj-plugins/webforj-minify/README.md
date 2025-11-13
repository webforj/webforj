# webforJ Minify Plugin

Build-time asset minification system for webforJ applications that automatically discovers and minifies CSS and JavaScript resources referenced via webforJ annotations.

### Maven Setup

Add the plugin to your `pom.xml`:

```xml
<build>
  <plugins>
    <!-- 1. Annotation Processor (discovers assets during compilation) -->
    <!-- NOTE: This configuration is required because annotation processing happens
         during compilation, before the minify plugin executes. Maven cannot
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
          <artifactId>webforj-minify-phcss-css</artifactId>
          <version>25.10-SNAPSHOT</version>
        </dependency>
        <!-- JavaScript minification -->
        <dependency>
          <groupId>com.webforj</groupId>
          <artifactId>webforj-minify-closure-js</artifactId>
          <version>25.10-SNAPSHOT</version>
        </dependency>
      </dependencies>
    </plugin>
  </plugins>
</build>
```

### Gradle Setup

**Step 1:** Configure plugin repositories in `settings.gradle.kts`:

> **Important for SNAPSHOT versions:** Gradle needs to know where to find the plugin. Add this configuration to your `settings.gradle.kts` file.

```kotlin
pluginManagement {
    repositories {
        mavenLocal()
        gradlePluginPortal()
        maven {
            name = "Central Portal Snapshots"
            url = uri("https://central.sonatype.com/repository/maven-snapshots/")
            mavenContent {
                snapshotsOnly()
            }
        }
    }
}

rootProject.name = "your-project-name"
```

**Step 2:** Add the plugin to your `build.gradle.kts`:

```kotlin
plugins {
  java
  id("com.webforj.minify") version "25.10-SNAPSHOT"
}

dependencies {
  // Minify foundation (provided scope for annotation processing)
  annotationProcessor("com.webforj:webforj-minify-foundation:25.10-SNAPSHOT")

  // Minifier implementations - add to the webforjMinifier configuration
  add("webforjMinifier", "com.webforj:webforj-minify-phcss-css:25.10-SNAPSHOT")
  add("webforjMinifier", "com.webforj:webforj-minify-closure-js:25.10-SNAPSHOT")
}

// Optional configuration
webforjMinify {
  skip.set(false)  // Set to true to skip minification
}
```

**Running Minification:**

The `minify` task runs automatically before the `jar` or `war` tasks:

```bash
# Minification happens automatically during build
./gradlew build

# Run minification manually
./gradlew minify

# Skip minification (via extension)
./gradlew build  # with skip.set(true) in build.gradle.kts
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

### 2. Build-Time Minification (Maven Plugin)

The Maven plugin runs in the `process-classes` phase (after compilation, before packaging):

1. Loads minifier implementations via Java SPI
2. Reads the manifest file
3. Resolves URLs to filesystem paths
4. Minifies assets in parallel (for >10 files)
5. Writes minified content back to original files

### 3. URL Protocol Resolution

The plugin understands webforJ URL protocols:

| Protocol | Resolves To | Example |
|----------|-------------|---------|
| `ws://` or `webserver://` | `src/main/resources/static/` | `ws://css/app.css` → `static/css/app.css` |
| `context://` | `src/main/resources/` | `context://styles/app.css` → `styles/app.css` |

**Note:** URLs without a protocol (e.g., `css/app.css`) are treated as `static/` paths for minification purposes.

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

### Minifier-Specific Configuration

You can customize the behavior of individual minifiers by adding a `<minifierConfigurations>` section to the plugin configuration.

**Maven Example - Closure JS Minifier:**
```xml
<plugin>
  <groupId>com.webforj</groupId>
  <artifactId>webforj-minify-maven-plugin</artifactId>
  <version>25.10-SNAPSHOT</version>
  <configuration>
    <minifierConfigurations>
      <closureJs>
        <compilationLevel>ADVANCED_OPTIMIZATIONS</compilationLevel>
        <languageIn>ECMASCRIPT_2020</languageIn>
        <languageOut>ECMASCRIPT5</languageOut>
        <prettyPrint>false</prettyPrint>
      </closureJs>
    </minifierConfigurations>
  </configuration>
  <executions>
    <execution>
      <goals>
        <goal>minify</goal>
      </goals>
    </execution>
  </executions>
  <dependencies>
    <dependency>
      <groupId>com.webforj</groupId>
      <artifactId>webforj-minify-closure-js</artifactId>
      <version>25.10-SNAPSHOT</version>
    </dependency>
  </dependencies>
</plugin>
```

**Closure JS Compiler Options:**

- **`compilationLevel`** - Optimization level (default: `SIMPLE_OPTIMIZATIONS`)
  - `WHITESPACE_ONLY` - Only removes whitespace and comments
  - `SIMPLE_OPTIMIZATIONS` - Renames local variables, removes dead code (recommended)
  - `ADVANCED_OPTIMIZATIONS` - Aggressive optimization including function/property renaming

- **`languageIn`** - Input JavaScript version (default: `ECMASCRIPT_NEXT`)
  - `ECMASCRIPT3`, `ECMASCRIPT5`, `ECMASCRIPT_2015` through `ECMASCRIPT_2021`, `ECMASCRIPT_NEXT`

- **`languageOut`** - Output JavaScript version (default: `ECMASCRIPT5`)
  - Same options as `languageIn`, plus `NO_TRANSPILE` to skip transpilation

- **`prettyPrint`** - Preserve formatting for debugging (default: `false`)

**Debug Logging:**

Use Maven's `-X` flag to see detailed configuration and compilation information:
```bash
mvn clean install -X
```

This will show:
- Configuration values being applied
- Compilation level and language settings for each file
- Which minifiers are being used

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

### Extensibility

Create custom minifiers by implementing the `AssetMinifier` interface:

```java
public class JsonMinifier implements AssetMinifier {

  @Override
  public String minify(String content, Path sourceFile) throws MinificationException {
    ...
  }

  @Override
  public Set<String> getSupportedExtensions() {
    return Set.of("json");
  }
}
```

Register via SPI by creating `META-INF/services/com.webforj.minify.foundation.AssetMinifier`:

```
com.example.JsonMinifier
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
      <artifactId>webforj-minify-phcss-css</artifactId>
      <version>25.10-SNAPSHOT</version>
    </dependency>
  </dependencies>
</plugin>
```

## Provided Minifiers

### CSS Minifier (`webforj-minify-phcss-css`)

Uses ph-css 8.0.0. Automatically skips `*.min.css` files.

**Features:**
- Removes comments and whitespace
- Optimizes property values
- CSS3 support with @media queries and vendor prefixes
- Graceful error handling (returns original on parse failure)

### JavaScript Minifier (`webforj-minify-closure-js`)

Uses Google Closure Compiler v20250820. Automatically skips `*.min.js` and `*.mjs` files.

**Default Configuration:**
- Compilation level: `SIMPLE_OPTIMIZATIONS`
- Language in: `ECMASCRIPT_NEXT`
- Language out: `ECMASCRIPT5`

**Features:**
- Variable renaming and code optimization
- Dead code elimination
- ES6+ syntax support
- Graceful error handling (returns original on compilation errors)

## Troubleshooting

### No minifiers registered via SPI

**Error:**
```
[WARN] No minifiers registered via SPI. Skipping minification.
[WARN] Ensure minifier modules are on the classpath.
```

**Solution:** Add minifier module dependencies to the plugin configuration:
- For CSS: Add `webforj-minify-phcss-css` as a plugin dependency
- For JavaScript: Add `webforj-minify-closure-js` as a plugin dependency

See Quick Start section for complete configuration examples.

### Manifest file not found

**Issue:** Plugin can't find `META-INF/webforj-resources.json`

**Solutions:**
1. Ensure annotation processor is configured correctly in `maven-compiler-plugin` (add `webforj-minify-foundation` to `annotationProcessorPaths`)
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

# Specific module
cd webforj-minify-foundation
mvn test
```