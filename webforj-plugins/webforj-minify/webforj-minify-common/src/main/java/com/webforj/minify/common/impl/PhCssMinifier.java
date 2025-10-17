package com.webforj.minify.common.impl;

import com.helger.css.ECSSVersion;
import com.helger.css.decl.CascadingStyleSheet;
import com.helger.css.reader.CSSReader;
import com.helger.css.writer.CSSWriter;
import com.helger.css.writer.CSSWriterSettings;
import com.webforj.minify.common.AssetMinifier;
import com.webforj.minify.common.MinificationException;

import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.Set;
import java.util.logging.Logger;

/**
 * CSS minifier implementation using the ph-css library.
 *
 * This implementation preserves semantic correctness and handles parse errors gracefully.
 */
public class PhCssMinifier implements AssetMinifier {
  private static final Logger LOGGER = Logger.getLogger(PhCssMinifier.class.getName());

  @Override
  public String minify(String content, Path sourceFile) throws MinificationException {
    try {
      // Parse CSS
      CascadingStyleSheet css = CSSReader.readFromString(content, StandardCharsets.UTF_8, ECSSVersion.CSS30);

      if (css == null) {
        LOGGER.warning(String.format(
          "Failed to parse CSS file %s. Returning original content.",
          sourceFile
        ));
        return content;
      }

      // Write minified CSS
      CSSWriterSettings settings = new CSSWriterSettings(ECSSVersion.CSS30, true);
      settings.setOptimizedOutput(true);
      settings.setRemoveUnnecessaryCode(true);

      CSSWriter writer = new CSSWriter(settings);
      return writer.getCSSAsString(css);

    } catch (Exception e) {
      LOGGER.warning(String.format(
        "Error minifying CSS file %s: %s. Returning original content.",
        sourceFile,
        e.getMessage()
      ));
      return content;
    }
  }

  @Override
  public Set<String> getSupportedExtensions() {
    return Set.of("css");
  }
}
