package com.webforj.minify.js;

import com.google.javascript.jscomp.CompilationLevel;
import com.google.javascript.jscomp.Compiler;
import com.google.javascript.jscomp.CompilerOptions;
import com.google.javascript.jscomp.CompilerOptions.LanguageMode;
import com.google.javascript.jscomp.SourceFile;
import com.webforj.minify.common.AssetMinifier;
import com.webforj.minify.common.MinificationException;
import java.nio.file.Path;
import java.util.Map;
import java.util.Set;

/**
 * JavaScript minifier implementation using Google Closure Compiler.
 *
 * <p>Configuration can be provided via the build plugin using the "closureJs" key. Default
 * configuration uses SIMPLE_OPTIMIZATIONS with ECMASCRIPT_NEXT input and ECMASCRIPT5 output.
 *
 * @author Kevin Hagel
 */
public class ClosureJsMinifier implements AssetMinifier {
  private static final System.Logger LOGGER = System.getLogger(ClosureJsMinifier.class.getName());

  // Default configuration values
  private String compilationLevel = "SIMPLE_OPTIMIZATIONS";
  private String languageIn = "ECMASCRIPT_NEXT";
  private String languageOut = "ECMASCRIPT5";
  private boolean prettyPrint = false;

  @Override
  public void configure(Map<String, Object> config) {
    if (config == null || !config.containsKey("closureJs")) {
      LOGGER.log(System.Logger.Level.DEBUG,
          "No closureJs configuration found, using defaults");
      return;
    }

    Object closureJsConfig = config.get("closureJs");
    if (!(closureJsConfig instanceof Map)) {
      LOGGER.log(System.Logger.Level.WARNING,
          "closureJs configuration is not a Map, using defaults");
      return;
    }

    @SuppressWarnings("unchecked")
    Map<String, Object> options = (Map<String, Object>) closureJsConfig;
    LOGGER.log(System.Logger.Level.DEBUG,
        "Configuring ClosureJS with options: " + options.keySet());

    if (options.containsKey("compilationLevel")) {
      this.compilationLevel = String.valueOf(options.get("compilationLevel"));
      LOGGER.log(System.Logger.Level.DEBUG,
          "Set compilationLevel: " + this.compilationLevel);
    }

    if (options.containsKey("languageIn")) {
      this.languageIn = String.valueOf(options.get("languageIn"));
      LOGGER.log(System.Logger.Level.DEBUG, "Set languageIn: " + this.languageIn);
    }

    if (options.containsKey("languageOut")) {
      this.languageOut = String.valueOf(options.get("languageOut"));
      LOGGER.log(System.Logger.Level.DEBUG, "Set languageOut: " + this.languageOut);
    }

    if (options.containsKey("prettyPrint")) {
      this.prettyPrint = Boolean.parseBoolean(String.valueOf(options.get("prettyPrint")));
      LOGGER.log(System.Logger.Level.DEBUG, "Set prettyPrint: " + this.prettyPrint);
    }
  }

  @Override
  public String minify(String content, Path sourceFile) throws MinificationException {
    try {
      final Compiler compiler = new Compiler();

      CompilerOptions options = new CompilerOptions();

      // Set language input from configuration
      LanguageMode langIn = parseLanguageMode(languageIn, LanguageMode.ECMASCRIPT_NEXT);
      options.setLanguageIn(langIn);
      LOGGER.log(System.Logger.Level.DEBUG,
          String.format("Using language in: %s for %s", langIn, sourceFile.getFileName()));

      // Set language output from configuration
      LanguageMode langOut = parseLanguageMode(languageOut, LanguageMode.ECMASCRIPT5);
      options.setLanguageOut(langOut);
      LOGGER.log(System.Logger.Level.DEBUG,
          String.format("Using language out: %s for %s", langOut, sourceFile.getFileName()));

      // Set compilation level from configuration
      CompilationLevel level = parseCompilationLevel(compilationLevel,
          CompilationLevel.SIMPLE_OPTIMIZATIONS);
      level.setOptionsForCompilationLevel(options);
      LOGGER.log(System.Logger.Level.DEBUG, String.format("Using compilation level: %s for %s",
          compilationLevel, sourceFile.getFileName()));

      // Set pretty print from configuration
      if (prettyPrint) {
        options.setPrettyPrint(true);
        LOGGER.log(System.Logger.Level.DEBUG,
            String.format("Pretty print enabled for %s", sourceFile.getFileName()));
      }

      // Disable warnings for third-party code
      options.setWarningLevel(com.google.javascript.jscomp.DiagnosticGroups.NON_STANDARD_JSDOC,
          com.google.javascript.jscomp.CheckLevel.OFF);

      SourceFile input = SourceFile.fromCode(sourceFile.toString(), content);
      SourceFile externs = SourceFile.fromCode("externs.js", "");

      compiler.compile(externs, input, options);

      if (compiler.hasErrors()) {
        LOGGER.log(System.Logger.Level.WARNING,
            String.format("Compilation errors in %s: %s. Returning original content.", sourceFile,
                compiler.getErrors()));
        return content;
      }

      return compiler.toSource();

    } catch (Exception e) {
      LOGGER.log(System.Logger.Level.WARNING,
          String.format("Error minifying JavaScript file %s: %s. Returning original content.",
              sourceFile, e.getMessage()));
      return content;
    }
  }

  @Override
  public Set<String> getSupportedExtensions() {
    return Set.of("js", "mjs");
  }

  @Override
  public boolean shouldMinify(Path filePath) {
    String fileName = filePath.getFileName().toString().toLowerCase();
    if (fileName.endsWith(".min.js") || fileName.endsWith(".min.mjs")) {
      LOGGER.log(System.Logger.Level.DEBUG,
          String.format("Skipping already minified file: %s", filePath.getFileName()));
      return false;
    }
    return true;
  }

  /**
   * Parses a string into a CompilationLevel enum.
   *
   * @param level the string representation of the compilation level
   * @param defaultLevel the default level to use if parsing fails
   * @return the parsed CompilationLevel
   */
  private CompilationLevel parseCompilationLevel(String level, CompilationLevel defaultLevel) {
    try {
      return CompilationLevel.valueOf(level);
    } catch (IllegalArgumentException e) {
      LOGGER.log(System.Logger.Level.WARNING, String.format(
          "Invalid compilation level '%s', using default %s", level, defaultLevel));
      return defaultLevel;
    }
  }

  /**
   * Parses a string into a LanguageMode enum.
   *
   * @param mode the string representation of the language mode
   * @param defaultMode the default mode to use if parsing fails
   * @return the parsed LanguageMode
   */
  private LanguageMode parseLanguageMode(String mode, LanguageMode defaultMode) {
    try {
      return LanguageMode.valueOf(mode);
    } catch (IllegalArgumentException e) {
      LOGGER.log(System.Logger.Level.WARNING,
          String.format("Invalid language mode '%s', using default %s", mode, defaultMode));
      return defaultMode;
    }
  }
}
