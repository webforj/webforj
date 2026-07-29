package com.webforj.devtools.craftforj.styles.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.styles.StylesheetModifier;
import com.webforj.devtools.craftforj.styles.StylesheetRegions;
import com.webforj.devtools.craftforj.styles.StylesheetResolver;
import com.webforj.devtools.craftforj.styles.model.StylesheetInfo;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@DisplayName("ReadStylesheetAction")
class ReadStylesheetActionTest {

  @TempDir
  Path projectRoot;

  private ReadStylesheetAction createAction() {
    StylesheetResolver resolver = new StylesheetResolver(projectRoot);
    return new ReadStylesheetAction(resolver, new StylesheetModifier());
  }

  @Test
  @DisplayName("Should return the default stylesheet with content")
  void shouldReturnDefaultStylesheet() throws IOException {
    Path file = projectRoot.resolve(StylesheetResolver.STATIC_STYLESHEET);
    Files.createDirectories(file.getParent());
    Files.writeString(file, "body {}\n");

    StylesheetInfo info = createAction().handle(new JsonObject());

    assertEquals(file.normalize().toString(), info.getPath());
    assertTrue(info.isExists());
    assertTrue(info.isDefaultUsed());
    assertEquals("body {}\n", info.getContent());
    assertEquals(StylesheetModifier.version("body {}\n"), info.getVersion());
  }

  @Test
  @DisplayName("Should report a missing stylesheet")
  void shouldReportMissingStylesheet() {
    StylesheetInfo info = createAction().handle(new JsonObject());

    assertFalse(info.isExists());
    assertNull(info.getContent());
    assertEquals(StylesheetModifier.version(null), info.getVersion());
  }

  @Test
  @DisplayName("Should read back the regions the file carries")
  void shouldReadRegions() throws IOException {
    Path file = projectRoot.resolve(StylesheetResolver.STATIC_STYLESHEET);
    Files.createDirectories(file.getParent());
    Files.writeString(file, "body {}\n" + StylesheetRegions.open("theme") + "\n:root { --x: 1; }\n"
        + StylesheetRegions.close("theme") + "\n");

    StylesheetInfo info = createAction().handle(new JsonObject());

    assertEquals(":root { --x: 1; }", info.getRegions().get("theme"));
  }

  @Test
  @DisplayName("Should report no regions for a file that carries none")
  void shouldReportNoRegions() throws IOException {
    Path file = projectRoot.resolve(StylesheetResolver.STATIC_STYLESHEET);
    Files.createDirectories(file.getParent());
    Files.writeString(file, "body {}\n");

    assertTrue(createAction().handle(new JsonObject()).getRegions().isEmpty());
  }

  @Test
  @DisplayName("Should use the configured file parameter")
  void shouldUseConfiguredFile() throws IOException {
    Path file = projectRoot.resolve("styles/custom.css");
    Files.createDirectories(file.getParent());
    Files.writeString(file, "a {}\n");

    JsonObject params = new JsonObject();
    params.addProperty("file", "styles/custom.css");

    StylesheetInfo info = createAction().handle(params);

    assertEquals(file.normalize().toString(), info.getPath());
    assertTrue(info.isExists());
    assertFalse(info.isDefaultUsed());
    assertEquals("a {}\n", info.getContent());
  }
}
