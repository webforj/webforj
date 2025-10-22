# webforj-minify Constitution

This document defines the core principles and coding standards for the webforj-minify project.

## Version Control

### Git Commits

**Never add "Co-Authored-By: Claude" or "Generated with Claude Code" to commit messages**

Since this is a public repository, commit messages must not include AI attribution. Commits should be attributed to human developers only.

**Correct:**
```
fix: add webforj-minify-gradle-plugin to report-aggregate module

Include Gradle plugin in JaCoCo coverage aggregation to ensure all minify plugin tests run on CI pipeline.
```

**Incorrect:**
```
fix: add webforj-minify-gradle-plugin to report-aggregate module

Include Gradle plugin in JaCoCo coverage aggregation.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>
```

**Rationale:** This is a company policy for public repositories to maintain professional commit history and proper human attribution.

### Pre-Commit Validation

**Always run `mvn checkstyle:check` before committing and pushing code**

All code changes must pass checkstyle validation before being committed to the repository. This ensures consistent code quality and prevents CI failures.

**Required command:**
```bash
mvn checkstyle:check
```

**Workflow:**
1. Make code changes
2. Run `mvn checkstyle:check` to validate
3. Fix any checkstyle violations
4. Commit and push only after checkstyle passes

**Why this matters:**
- Prevents CI pipeline failures due to style violations
- Maintains consistent code formatting across the project
- Catches issues early in the development process
- Reduces review cycles and rework

**Rationale:** Checkstyle violations that reach CI waste time and resources. Running checkstyle locally before committing ensures code quality standards are met before code review.

## Coding Standards

### Logging

**Use `System.getLogger()` instead of `java.util.logging.Logger.getLogger()`**

To maintain consistency with the webforJ codebase, all logging must use the Java Platform Logging API (`System.Logger`) rather than the legacy `java.util.logging.Logger`.

**Correct:**
```java
import java.lang.System.Logger;

public class MyClass {
    private static final System.Logger LOGGER = System.getLogger(MyClass.class.getName());

    public void myMethod() {
        LOGGER.log(System.Logger.Level.INFO, "This is the correct way");
    }
}
```

**Incorrect:**
```java
import java.util.logging.Logger;

public class MyClass {
    private static final Logger LOGGER = Logger.getLogger(MyClass.class.getName());

    public void myMethod() {
        LOGGER.info("This is inconsistent with webforJ standards");
    }
}
```

**Rationale:** The Java Platform Logging API (`System.Logger`) provides a facade that allows the underlying logging framework to be configured at deployment time, offering better flexibility and consistency across the webforJ ecosystem.

## Architecture

### Module Structure

The webforj-minify project uses a modular architecture with clear separation of concerns:

**Core Modules:**
- `webforj-minify-common` - Core interfaces (AssetMinifier), utilities (ResourceResolver, MinifierRegistry), and annotation processor
- `webforj-minify-css` - CSS minification implementation using ph-css
- `webforj-minify-js` - JavaScript minification implementation using Google Closure Compiler
- `webforj-minify-maven-plugin` - Maven plugin implementation (depends only on common)
- `webforj-minify-gradle-plugin` - Gradle plugin implementation (depends only on common)

**Dependency Rules:**
- The Maven/Gradle plugins MUST depend only on `webforj-minify-common`
- CSS and JS modules MUST NOT be compile-time dependencies of the plugins
- Users declare which minifiers they want by adding them as plugin dependencies in their projects

### Pluggable Minifier System

The project uses Java Service Provider Interface (SPI) for pluggable minifier implementations:

**Core Contract:**
```java
public interface AssetMinifier {
    String minify(String content, Path sourceFile) throws MinificationException;
    Set<String> getSupportedExtensions();
}
```

**Service Registration:**
Each minifier implementation JAR must include `META-INF/services/com.webforj.minify.common.AssetMinifier` listing the implementation class:
```
com.webforj.minify.css.PhCssMinifier
```

**Discovery:**
The `MinifierRegistry` uses `ServiceLoader.load(AssetMinifier.class, classLoader)` to discover all available minifiers from the plugin's classpath.

**Extension Points:**
Users can create custom minifiers (e.g., for SASS, LESS, SVG) by:
1. Implementing the `AssetMinifier` interface
2. Registering via SPI in their JAR
3. Adding their JAR as a plugin dependency

**Rationale:** This architecture ensures:
- Zero coupling between the plugin and specific minifier implementations
- Users only download dependencies for minifiers they actually use
- Custom minifiers integrate seamlessly without modifying plugin code
- Clear separation allows independent versioning of minifier implementations
