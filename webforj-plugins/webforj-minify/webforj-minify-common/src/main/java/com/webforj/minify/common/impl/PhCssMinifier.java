package com.webforj.minify.common.impl;

import com.helger.css.decl.CascadingStyleSheet;
import com.helger.css.reader.CSSReader;
import com.helger.css.writer.CSSWriter;
import com.helger.css.writer.CSSWriterSettings;
import com.webforj.minify.common.AssetMinifier;
import com.webforj.minify.common.MinificationException;
import java.nio.file.Path;
import java.util.Set;
import java.util.logging.Logger;

/**
 * CSS minifier implementation using the ph-css library (v8.0.0).
 *
 * <p>
 * This implementation preserves semantic correctness and handles parse errors gracefully by
 * returning the original content when minification fails.
 */
public class PhCssMinifier implements AssetMinifier {
  private static final Logger LOGGER = Logger.getLogger(PhCssMinifier.class.getName());

  @Override
  public String minify(String content, Path sourceFile) throws MinificationException {
    try {
      // Parse CSS (ph-css 8.0.0 API - using non-deprecated method)
      CascadingStyleSheet css = CSSReader.readFromString(content);

      if (css == null) {
        LOGGER.warning(
            String.format("Failed to parse CSS file %s. Returning original content.", sourceFile));
        return content;
      }

      // Write minified CSS (optimized output)
      CSSWriterSettings settings = new CSSWriterSettings();
      settings.setOptimizedOutput(true);
      settings.setRemoveUnnecessaryCode(true);

      CSSWriter writer = new CSSWriter(settings);
      String minified = writer.getCSSAsString(css);

      LOGGER.fine(String.format("Minified %s: %d bytes -> %d bytes", sourceFile.getFileName(),
          content.length(), minified.length()));

      return minified;

    } catch (Exception e) {
      LOGGER.warning(String.format("Error minifying CSS file %s: %s. Returning original content.",
          sourceFile, e.getMessage()));
      return content;
    }
  }

  @Override
  public Set<String> getSupportedExtensions() {
    return Set.of("css");
  }
}
