package com.webforj.devtools.craftforj.styles;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.devtools.craftforj.action.CraftforjActionException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class StylesheetResolverTest {

  @TempDir
  Path projectRoot;

  private Path createFile(String relativePath) throws IOException {
    Path file = projectRoot.resolve(relativePath);
    Files.createDirectories(file.getParent());
    Files.writeString(file, "body {}\n");

    return file;
  }

  @Nested
  @DisplayName("defaultPath")
  class DefaultPath {

    @Test
    @DisplayName("Should prefer the frontend stylesheet on the frontend layout")
    void shouldPreferFrontendStylesheet() throws IOException {
      Path frontend = createFile(StylesheetResolver.FRONTEND_STYLESHEET);
      createFile(StylesheetResolver.STATIC_STYLESHEET);

      StylesheetResolver resolver = new StylesheetResolver(projectRoot);

      assertEquals(frontend.normalize(), resolver.defaultPath());
    }

    @Test
    @DisplayName("Should use the frontend stylesheet when only it exists")
    void shouldUseFrontendStylesheetWhenOnlyItExists() throws IOException {
      Path frontend = createFile(StylesheetResolver.FRONTEND_STYLESHEET);

      StylesheetResolver resolver = new StylesheetResolver(projectRoot);

      assertEquals(frontend.normalize(), resolver.defaultPath());
    }

    @Test
    @DisplayName("Should fall back to the other convention when only that one exists")
    void shouldFallBackToExistingConvention() throws IOException {
      Path staticFile = createFile(StylesheetResolver.STATIC_STYLESHEET);

      StylesheetResolver resolver = new StylesheetResolver(projectRoot);

      assertEquals(staticFile.normalize(), resolver.defaultPath());
    }

    @Test
    @DisplayName("Should return the layout default when nothing exists")
    void shouldReturnLayoutDefaultWhenNothingExists() {
      StylesheetResolver resolver = new StylesheetResolver(projectRoot);

      assertEquals(projectRoot.resolve(StylesheetResolver.FRONTEND_STYLESHEET).normalize(),
          resolver.defaultPath());
    }
  }

  @Nested
  @DisplayName("resolve")
  class Resolve {

    @Test
    @DisplayName("Should resolve a relative configured path against the project root")
    void shouldResolveRelativeConfiguredPath() {
      StylesheetResolver resolver = new StylesheetResolver(projectRoot);

      assertEquals(projectRoot.resolve("styles/custom.css").normalize(),
          resolver.resolve("styles/custom.css"));
    }

    @Test
    @DisplayName("Should use the default when the configured path is blank")
    void shouldUseDefaultWhenConfiguredPathBlank() {
      StylesheetResolver resolver = new StylesheetResolver(projectRoot);

      assertEquals(resolver.defaultPath(), resolver.resolve("  "));
      assertEquals(resolver.defaultPath(), resolver.resolve(null));
    }

    @Test
    @DisplayName("Should reject an absolute configured path")
    void shouldRejectAbsoluteConfiguredPath() {
      StylesheetResolver resolver = new StylesheetResolver(projectRoot);
      String absolute = projectRoot.resolve("styles/custom.css").toString();

      assertThrows(CraftforjActionException.class, () -> resolver.resolve(absolute));
    }

    @Test
    @DisplayName("Should reject a path that escapes the project root")
    void shouldRejectPathEscapingProjectRoot() {
      StylesheetResolver resolver = new StylesheetResolver(projectRoot);

      assertThrows(CraftforjActionException.class, () -> resolver.resolve("../outside.css"));
      assertThrows(CraftforjActionException.class,
          () -> resolver.resolve("styles/../../outside.css"));
    }

    @Test
    @DisplayName("Should reject a path that is not a css file")
    void shouldRejectNonCssPath() {
      StylesheetResolver resolver = new StylesheetResolver(projectRoot);

      assertThrows(CraftforjActionException.class, () -> resolver.resolve("src/main/App.java"));
    }
  }
}
