package com.webforj.minify.common.impl;

import com.google.javascript.jscomp.CompilationLevel;
import com.google.javascript.jscomp.Compiler;
import com.google.javascript.jscomp.CompilerOptions;
import com.google.javascript.jscomp.SourceFile;
import com.google.javascript.jscomp.CompilerOptions.LanguageMode;
import com.webforj.minify.common.AssetMinifier;
import com.webforj.minify.common.MinificationException;

import java.nio.file.Path;
import java.util.Set;
import java.util.logging.Logger;

/**
 * JavaScript minifier implementation using Google Closure Compiler.
 *
 * Uses SIMPLE_OPTIMIZATIONS for safe minification without symbol renaming.
 */
public class ClosureJsMinifier implements AssetMinifier {
  private static final Logger LOGGER = Logger.getLogger(ClosureJsMinifier.class.getName());

  @Override
  public String minify(String content, Path sourceFile) throws MinificationException {
    try {
      Compiler compiler = new Compiler();

      CompilerOptions options = new CompilerOptions();
      // Use ECMASCRIPT_NEXT for input (modern JS)
      options.setLanguageIn(LanguageMode.ECMASCRIPT_NEXT);
      // Use ECMASCRIPT5 for output (broad browser compatibility)
      options.setLanguageOut(LanguageMode.ECMASCRIPT5);
      // Use SIMPLE_OPTIMIZATIONS (no symbol renaming)
      CompilationLevel.SIMPLE_OPTIMIZATIONS.setOptionsForCompilationLevel(options);
      // Disable warnings for third-party code
      options.setWarningLevel(com.google.javascript.jscomp.DiagnosticGroups.NON_STANDARD_JSDOC,
                               com.google.javascript.jscomp.CheckLevel.OFF);

      SourceFile input = SourceFile.fromCode(sourceFile.toString(), content);
      SourceFile externs = SourceFile.fromCode("externs.js", "");

      compiler.compile(externs, input, options);

      if (compiler.hasErrors()) {
        LOGGER.warning(String.format(
          "Compilation errors in %s: %s. Returning original content.",
          sourceFile,
          compiler.getErrors()
        ));
        return content;
      }

      return compiler.toSource();

    } catch (Exception e) {
      LOGGER.warning(String.format(
        "Error minifying JavaScript file %s: %s. Returning original content.",
        sourceFile,
        e.getMessage()
      ));
      return content;
    }
  }

  @Override
  public Set<String> getSupportedExtensions() {
    return Set.of("js");
  }
}
