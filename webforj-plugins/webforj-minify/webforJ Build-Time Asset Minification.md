webforJ Build-Time Asset Minification \- Project Requirements Objective   
Implement a build-time asset minification system that processes CSS and JavaScript files during the final build phase, reducing file sizes for production deployments. The system must integrate with both Maven and Gradle build lifecycles without requiring runtime overhead or framework changes. 

Problem Statement 

webforJ applications reference CSS/JS assets via annotations ( @StyleSheet , @JavaScript , @InlineStyleSheet , @InlineJavaScript ). Currently, these assets are served unminified, resulting in: 

Larger WAR file sizes   
Increased network transfer time   
Slower application initialization 

Developers need a build plugin that: 

1\. Discovers assets referenced in annotations during compilation   
2\. Minifies those assets during the build process   
3\. Replaces the original files with minified versions in the final artifact   
4\. Works identically for both Maven and Gradle builds   
5\. Allows swapping minification engines without code changes 

Functional Requirements 

FR-1: Asset Discovery via Annotation Processing 

Scan source code during compilation for webforJ asset annotations   
Extract resource paths from @StyleSheet , @JavaScript , @InlineStyleSheet , @InlineJavaScript   
Generate a JSON manifest file listing all discovered assets   
Manifest location: META-INF/webforj-resources.json   
Manifest format: 

{   
"resources": \[   
{   
"url": "webserver://css/app.css",   
"type": "StyleSheet"   
},   
{   
"url": "ws://js/app.js",   
"type": "JavaScript"   
},   
{   
"url": "context://templates/styles.css",   
"type": "InlineStyleSheet"   
}   
\]   
} 

FR-2: Pluggable Minification Architecture 

Design Principle: The minification system MUST use a pluggable architecture where minifier implementations can be replaced without modifying plugin code. 

Minifier Interface Design 

Define interfaces that decouple the plugin from specific minification libraries:  
public interface AssetMinifier { 

/\*\* 

\* Minifies the content of an asset file. 

\* 

\* @param content the original file content 

\* @param sourceFile the source file path (for error reporting) 

\* @return minified content 

\* @throws MinificationException if minification fails 

\*/ 

String minify(String content, Path sourceFile) throws MinificationException; 

/\*\* 

\* Returns the file extensions this minifier supports. 

\* 

\* @return set of extensions without dot (e.g., \["css", "scss"\]) 

\*/ 

Set\<String\> getSupportedExtensions(); 

} 

Default Implementations 

CSS Minifier: 

public class PhCssMinifier implements AssetMinifier { 

// Uses ph-css library 

// Dependency: com.helger:ph-css:7.0.2 

} 

JavaScript Minifier: 

public class ClosureJsMinifier implements AssetMinifier { 

// Uses Google Closure Compiler 

// Dependency: com.google.javascript:closure-compiler:v20231112 

} 

Minifier Registry 

The plugin uses a registry to discover and instantiate minifiers: 

public class MinifierRegistry { 

private final Map\<String, AssetMinifier\> minifiers \= new HashMap\<\>(); 

public void register(AssetMinifier minifier); 

public Optional\<AssetMinifier\> getMinifier(String fileExtension); 

} 

Custom Minifier Registration (Java SPI) 

Custom minifiers are automatically discovered via Java Service Provider Interface. No plugin configuration required. Steps to add a custom minifier: 

1\. Implement the AssetMinifier interface   
2\. Create META-INF/services/com.webforj.minify.common.AssetMinifier in your JAR 3\. List your implementation class(es) in the service file   
4\. Add your minifier JAR as a dependency to the project 

The plugin automatically discovers and uses custom minifiers via ServiceLoader . 

Requirements for Pluggable Design 

1\. Default minifiers (ph-css, Closure Compiler) must be isolated in separate implementation classes 2\. Default minifiers registered as SPI providers in webforj-minify-common module 

3\. Plugin core logic must only depend on the AssetMinifier interface   
4\. Custom minifier discovery via ServiceLoader.load(AssetMinifier.class, classLoader) 5\. If service loading fails for a minifier, log error and skip that minifier   
6\. If no minifier found for an extension, use default implementation   
7\. Minifiers must be stateless (thread-safe for parallel processing)  
FR-3: Asset Processing 

Process files in-place, replacing originals   
Only process files with .css or .js extensions   
Skip files already minified (pattern: \*.min.css , \*.min.js )   
Use UTF-8 encoding for all file I/O   
Preserve file modification timestamps where possible 

FR-4: Additional Asset Configuration 

Support a configuration file for assets not discoverable via annotations   
Location: src/main/resources/META-INF/webforj-minify.txt   
Format: One pattern per line, use \! prefix for exclusions   
Supports glob patterns using Java NIO PathMatcher syntax   
Comments start with \#   
Empty lines are ignored   
Example: 

\# Include patterns \- minify these files   
static/css/theme-\*.css   
static/js/modules/\*.js   
context://templates/\*.css 

\# Exclude patterns \- skip minification for these files   
\!static/js/vendor/\*.js   
\!static/css/lib/\*.css 

FR-5: Protocol Resolution 

Map webforJ resource protocols to filesystem paths: 

webserver://path → src/main/resources/static/path   
ws://path → src/main/resources/static/path   
context://path → src/main/resources/path   
No protocol → treat as static/path   
https://docs.webforj.com/docs/managing-resources/assets-protocols 

FR-6: Build System Integration 

Maven Plugin 

Mojo bound to process-classes phase (runs after compilation, before WAR packaging) Plugin coordinates: com.webforj:webforj-minify-maven-plugin 

Configuration-free operation (convention over configuration)   
Optional profile-based activation for production builds only   
Support for custom minifier configuration 

Gradle Plugin 

Task runs after processResources , before jar / war tasks   
Plugin ID: com.webforj.minify   
Auto-applies when webforJ dependency is detected   
Gradle configuration DSL for enabling/disabling minification   
Support for custom minifier configuration via extension 

FR-7: Error Handling 

Continue build on minification errors (log warnings, keep original file)   
Skip missing files with warning messages   
Skip non-CSS/JS files silently   
Fail build on malformed manifest file   
Log clearly which minifier implementation is being used for each file type Technical Requirements 

TR-1: Module Structure  
webforj-plugins/webforj-minify/ 

├── webforj-minify-common/ \# Shared constants, interfaces, and utilities 

│ ├── AssetMinifier.java \# Core minifier interface 

│ ├── MinifierRegistry.java \# Minifier discovery and registration 

│ ├── MinificationException.java \# Exception types 

│ └── ResourceResolver.java \# Protocol resolution logic 

├── webforj-minify-processor/ \# Java annotation processor (APT) 

├── webforj-minify-maven-plugin/ \# Maven Mojo implementation 

└── webforj-minify-gradle-plugin/ \# Gradle Task implementation 

TR-2: Annotation Processor Implementation 

Generates META-INF/webforj-resources.json in output directory (JSON format)   
Processes class-level annotations only (no method/field scanning)   
Incremental compilation safe (no cross-file dependencies)   
Uses Gson for JSON generation 

TR-3: Default Minifier Implementations 

CSS Minification (PhCssMinifier) 

Dependency: 

\<dependency\> 

\<groupId\>com.helger\</groupId\> 

\<artifactId\>ph-css\</artifactId\> 

\<version\>\[version\]\</version\> 

\<optional\>true\</optional\> 

\</dependency\> 

Implementation Requirements: 

Preserve semantic correctness (no aggressive optimizations that break compatibility)   
Handle parse errors gracefully (return original content, log error) 

JavaScript Minification (ClosureJsMinifier) 

Dependency: 

\<dependency\> 

\<groupId\>com.google.javascript\</groupId\> 

\<artifactId\>closure-compiler\</artifactId\> 

\<version\>\[version\]\</version\> 

\<optional\>true\</optional\> 

\</dependency\> 

Implementation Requirements: 

Use ECMASCRIPT\_NEXT input language mode   
Use ECMASCRIPT5 output language mode (broad browser compatibility)   
Use SIMPLE\_OPTIMIZATIONS compilation level   
No symbol renaming (preserve API contracts)   
Disable warnings for third-party code   
Handle compilation errors gracefully (return original content, log error) 

Important: Both minifier dependencies must be marked as \<optional\>true\</optional\> so projects using custom minifiers don't need to pull in unused libraries. 

TR-6: File Processing Rules 

1\. Only process files with .css or .js extensions   
2\. Skip files already matching \*.min.css or \*.min.js pattern   
3\. Preserve file modification timestamps where possible   
4\. Use UTF-8 encoding for all file I/O   
5\. Handle glob patterns in webforj-minify.txt (NIO PathMatcher syntax)   
6\. Process inclusion patterns first, then apply exclusion patterns (those starting with \! )   
7\. Process files in parallel when possible (use available CPU cores)   
8\. Atomic file writes (write to temp file, then rename) 

TR-7: Minifier Loading Mechanism (Java SPI)  
Both Maven and Gradle plugins MUST use Java SPI for discovering and loading minifier implementations. 

Service Provider Configuration: Custom minifier implementations register via META-INF/services/com.webforj.minify.common.AssetMinifier : 

\# Contents of META-INF/services/com.webforj.minify.common.AssetMinifier 

com.example.CustomCssMinifier 

com.example.CustomJsMinifier 

Loading Mechanism: 

1\. Use ServiceLoader.load(AssetMinifier.class) to discover all implementations on classpath   
2\. Iterate discovered services and register each with MinifierRegistry   
3\. Registry maps minifiers by their supported extensions (from getSupportedExtensions() )   
4\. If multiple minifiers support the same extension, last registered wins (log warning)   
5\. If no custom minifier found for an extension, use default implementation   
6\. Log all registered minifiers at INFO level: 

\[INFO\] Registered CSS minifier: com.example.CustomCssMinifier   
\[INFO\] Registered JS minifier: com.webforj.minify.ClosureJsMinifier (default) 

Annotation Extension (Prerequisite) 

Note: Current webforJ annotations ( @StyleSheet , @JavaScript , etc.) do not have a minify attribute. This feature will be controlled solely through: 

Properties file ( webforj-minify.properties )   
Build plugin configuration   
Future enhancement: add minify attribute to annotations 

No annotation changes required for initial version. 

Out of Scope 

The following are explicitly NOT part of this implementation: 

Runtime minification or on-the-fly processing   
Source map generation   
Asset bundling (combining multiple files)   
Image optimization or other asset types   
CDN integration or asset hashing   
Development mode with watch/rebuild   
Adding minify attribute to annotations (future enhancement)