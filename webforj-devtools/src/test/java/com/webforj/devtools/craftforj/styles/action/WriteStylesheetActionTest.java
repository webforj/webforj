package com.webforj.devtools.craftforj.styles.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.gson.JsonArray;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.styles.StylesheetModifier;
import com.webforj.devtools.craftforj.styles.StylesheetRegions;
import com.webforj.devtools.craftforj.styles.StylesheetResolver;
import com.webforj.devtools.craftforj.styles.model.StylesheetResult;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@DisplayName("WriteStylesheetAction")
class WriteStylesheetActionTest {

  @TempDir
  Path projectRoot;

  private WriteStylesheetAction action;
  private Path stylesheet;

  @BeforeEach
  void setUp() throws IOException {
    action =
        new WriteStylesheetAction(new StylesheetResolver(projectRoot), new StylesheetModifier());
    stylesheet = projectRoot.resolve(StylesheetResolver.FRONTEND_STYLESHEET);
    Files.createDirectories(stylesheet.getParent());
    Files.writeString(stylesheet, "body {\n  margin: 0;\n}\n");
  }

  private static JsonObject changeParams(JsonObject... changeObjects) {
    JsonArray changes = new JsonArray();
    for (JsonObject change : changeObjects) {
      changes.add(change);
    }

    JsonObject params = new JsonObject();
    params.add("changes", changes);

    return params;
  }

  private static JsonObject editChange(String oldText, String newText) {
    JsonObject change = new JsonObject();
    change.addProperty("type", "EDIT");
    change.addProperty("oldText", oldText);
    change.addProperty("newText", newText);

    return change;
  }

  private static JsonObject appendChange(String text) {
    JsonObject change = new JsonObject();
    change.addProperty("type", "APPEND");
    change.addProperty("text", text);

    return change;
  }

  private static JsonObject regionChange(String region, String text) {
    return regionChange(region, text, null);
  }

  private static JsonObject regionChange(String region, String text, String placement) {
    JsonObject change = new JsonObject();
    change.addProperty("type", "REGION");
    change.addProperty("region", region);
    if (text != null) {
      change.addProperty("text", text);
    }
    if (placement != null) {
      change.addProperty("placement", placement);
    }

    return change;
  }

  private static JsonObject replaceParams(String content, String baseVersion) {
    JsonObject change = new JsonObject();
    change.addProperty("type", "REPLACE");
    if (content != null) {
      change.addProperty("text", content);
    }

    JsonObject params = changeParams(change);
    if (baseVersion != null) {
      params.addProperty("baseVersion", baseVersion);
    }

    return params;
  }

  @Test
  @DisplayName("Should apply an edit change and write the stylesheet")
  void shouldApplyEdits() throws IOException {
    StylesheetResult result =
        action.handle(changeParams(editChange("margin: 0;", "margin: 1rem;")));

    assertEquals(null, result.getError());
    assertTrue(result.isApplied());
    assertEquals("body {\n  margin: 1rem;\n}\n", Files.readString(stylesheet));
  }

  @Test
  @DisplayName("Should preview without writing when dryRun is set")
  void shouldPreviewWithDryRun() throws IOException {
    JsonObject params = changeParams(editChange("margin: 0;", "margin: 1rem;"));
    params.addProperty("dryRun", true);

    StylesheetResult result = action.handle(params);

    assertEquals(null, result.getError());
    assertFalse(result.isApplied());
    assertEquals("body {\n  margin: 1rem;\n}\n", result.getContent());
    assertEquals("body {\n  margin: 0;\n}\n", Files.readString(stylesheet));
  }

  @Test
  @DisplayName("Should apply an append change")
  void shouldAppendBlock() throws IOException {
    StylesheetResult result = action.handle(changeParams(appendChange("a { color: red; }")));

    assertTrue(result.isApplied());
    assertEquals("body {\n  margin: 0;\n}\na { color: red; }\n", Files.readString(stylesheet));
  }

  @Test
  @DisplayName("Should target the configured file parameter")
  void shouldTargetConfiguredFile() throws IOException {
    Path custom = projectRoot.resolve("styles/custom.css");
    Files.createDirectories(custom.getParent());
    Files.writeString(custom, "a {}\n");

    JsonObject params = changeParams(appendChange("b {}"));
    params.addProperty("file", "styles/custom.css");

    StylesheetResult result = action.handle(params);

    assertTrue(result.isApplied());
    assertEquals("a {}\nb {}\n", Files.readString(custom));
  }

  @Test
  @DisplayName("Should report an error result for invalid changes")
  void shouldReportErrorForInvalidChanges() {
    StylesheetResult result = action.handle(changeParams(editChange("nope", "never")));

    assertFalse(result.isApplied());
    assertTrue(result.getError().contains("not found"));
  }

  @Test
  @DisplayName("writes a region and reads back as its own fenced block")
  void shouldWriteRegion() throws IOException {
    StylesheetResult result =
        action.handle(changeParams(regionChange("theme", ":root { --x: 1; }")));

    assertTrue(result.isApplied());
    String content = Files.readString(stylesheet);
    assertTrue(content.startsWith(StylesheetRegions.open("theme")),
        "a region goes in at the top, since it may carry an @import");
    assertTrue(content.contains("body {\n  margin: 0;\n}\n"));
    assertEquals(":root { --x: 1; }", StylesheetRegions.find(content, "theme"));
  }

  @Test
  @DisplayName("keeps a region below a charset, which has to stay first")
  void shouldKeepCharsetFirst() throws IOException {
    Files.writeString(stylesheet, "@charset \"utf-8\";\nbody {}\n");

    action.handle(changeParams(regionChange("theme", ":root { --x: 1; }")));

    String content = Files.readString(stylesheet);
    assertTrue(content.startsWith("@charset \"utf-8\";"));
    assertEquals(":root { --x: 1; }", StylesheetRegions.find(content, "theme"));
  }

  @Test
  @DisplayName("keeps a region below the imports the file opens with, which stay valid")
  void shouldKeepImportsFirst() throws IOException {
    String head = "@charset \"utf-8\";\n/* fonts */\n@import url('theirs.css');\n";
    Files.writeString(stylesheet, head + "body {}\n");

    action
        .handle(changeParams(regionChange("theme", "@import url('ours.css');\n:root { --x: 1; }")));

    String content = Files.readString(stylesheet);
    assertTrue(content.startsWith(head), "the file keeps opening with its own imports");
    assertTrue(content.indexOf("theirs.css") < content.indexOf(StylesheetRegions.open("theme")),
        "a rule before an import would make that import invalid");
    assertEquals("@import url('ours.css');\n:root { --x: 1; }",
        StylesheetRegions.find(content, "theme"));
  }

  @Test
  @DisplayName("replaces its own region without touching anything else")
  void shouldReplaceRegionInPlace() throws IOException {
    action.handle(changeParams(regionChange("theme", ":root { --x: 1; }")));
    action.handle(changeParams(appendChange("a { color: red; }")));
    StylesheetResult result =
        action.handle(changeParams(regionChange("theme", ":root { --x: 2; }")));

    assertTrue(result.isApplied());
    String content = Files.readString(stylesheet);
    assertEquals(":root { --x: 2; }", StylesheetRegions.find(content, "theme"));
    assertTrue(content.contains("body {\n  margin: 0;\n}"));
    assertTrue(content.contains("a { color: red; }"));
    assertFalse(content.contains("--x: 1"));
  }

  @Test
  @DisplayName("keeps the developer's own rules on their own line however often it is saved")
  void shouldKeepRegionOnItsOwnLines() throws IOException {
    for (int save = 0; save < 5; save++) {
      action.handle(changeParams(regionChange("theme", ":root { --x: " + save + "; }")));
    }

    String content = Files.readString(stylesheet);

    assertTrue(content.contains(StylesheetRegions.close("theme") + "\n"),
        "the rule after the region starts on a line of its own");
    assertTrue(content.contains("body {\n  margin: 0;\n}"));
  }

  @Test
  @DisplayName("takes its region back out when the text is blank")
  void shouldRemoveRegion() throws IOException {
    action.handle(changeParams(regionChange("theme", ":root { --x: 1; }")));
    StylesheetResult result = action.handle(changeParams(regionChange("theme", "")));

    assertTrue(result.isApplied());
    String content = Files.readString(stylesheet);
    assertEquals(null, StylesheetRegions.find(content, "theme"));
    assertFalse(content.contains("webforj-devtools:theme"));
    assertTrue(content.contains("body {\n  margin: 0;\n}"));
  }

  @Test
  @DisplayName("keeps two regions apart")
  void shouldKeepRegionsApart() throws IOException {
    action.handle(changeParams(regionChange("theme", ":root { --x: 1; }")));
    action.handle(changeParams(regionChange("assistant", "a { color: red; }")));
    action.handle(changeParams(regionChange("theme", ":root { --x: 2; }")));

    String content = Files.readString(stylesheet);
    assertEquals(":root { --x: 2; }", StylesheetRegions.find(content, "theme"));
    assertEquals("a { color: red; }", StylesheetRegions.find(content, "assistant"));
  }

  @Test
  @DisplayName("rejects a region without a usable name")
  void shouldRejectBadRegionName() throws IOException {
    StylesheetResult result = action.handle(changeParams(regionChange("Theme Tab", "a {}")));

    assertFalse(result.isApplied());
    assertTrue(result.getError().contains("not a usable name"));
    assertEquals("body {\n  margin: 0;\n}\n", Files.readString(stylesheet));
  }

  @Test
  @DisplayName("writes a region into a stylesheet that does not exist yet")
  void shouldCreateFileForRegion() throws IOException {
    Files.delete(stylesheet);

    StylesheetResult result =
        action.handle(changeParams(regionChange("theme", ":root { --x: 1; }")));

    assertTrue(result.isApplied());
    assertEquals(":root { --x: 1; }",
        StylesheetRegions.find(Files.readString(stylesheet), "theme"));
  }

  @Test
  @DisplayName("creates the stylesheet from the missing file version")
  void shouldCreateStylesheet() throws IOException {
    Files.delete(stylesheet);
    String content = "dwc-button {\n  background-color: red;\n}\n";

    StylesheetResult result =
        action.handle(replaceParams(content, StylesheetModifier.version(null)));

    assertTrue(result.isApplied());
    assertFalse(result.isConflict());
    assertEquals(content, Files.readString(stylesheet));
    assertEquals(StylesheetModifier.version(content), result.getVersion());
  }

  @Test
  @DisplayName("writes when the base version matches the file")
  void shouldWriteOnMatchingVersion() throws IOException {
    Files.writeString(stylesheet, "body { margin: 0; }\n");

    String updated = "body { margin: 0; }\ndwc-button { color: red; }\n";
    StylesheetResult result =
        action.handle(replaceParams(updated, StylesheetModifier.version("body { margin: 0; }\n")));

    assertTrue(result.isApplied());
    assertEquals(updated, Files.readString(stylesheet));
  }

  @Test
  @DisplayName("rejects a stale base version and returns the current state")
  void shouldRejectStaleVersion() throws IOException {
    Files.writeString(stylesheet, "body { margin: 0; }\n");

    StylesheetResult result = action
        .handle(replaceParams("dwc-button { color: red; }\n", StylesheetModifier.version("old")));

    assertFalse(result.isApplied());
    assertTrue(result.isConflict());
    assertEquals("body { margin: 0; }\n", result.getContent());
    assertEquals(StylesheetModifier.version("body { margin: 0; }\n"), result.getVersion());
    assertEquals("body { margin: 0; }\n", Files.readString(stylesheet));
  }

  @Test
  @DisplayName("guards any change with a base version, not only a replace")
  void shouldGuardEveryChangeWithVersion() throws IOException {
    JsonObject params = changeParams(appendChange("a { color: red; }"));
    params.addProperty("baseVersion", StylesheetModifier.version("something else"));

    StylesheetResult result = action.handle(params);

    assertFalse(result.isApplied());
    assertTrue(result.isConflict());
    assertEquals("body {\n  margin: 0;\n}\n", Files.readString(stylesheet));
  }

  @Test
  @DisplayName("rejects missing content")
  void shouldRejectMissingContent() throws IOException {
    StylesheetResult result =
        action.handle(replaceParams(null, StylesheetModifier.version("body {\n  margin: 0;\n}\n")));

    assertFalse(result.isApplied());
    assertTrue(result.getError().contains("needs text"));
    assertEquals("body {\n  margin: 0;\n}\n", Files.readString(stylesheet));
  }

  @Test
  @DisplayName("rejects a missing base version")
  void shouldRejectMissingBaseVersion() {
    assertThrows(CraftforjActionException.class,
        () -> action.handle(replaceParams("body {}\n", null)));
  }

  @Test
  @DisplayName("honors a configured stylesheet path")
  void shouldHonorConfiguredPath() throws IOException {
    JsonObject params = replaceParams("body {}\n", StylesheetModifier.version(null));
    params.addProperty("file", "styles/custom.css");

    StylesheetResult result = action.handle(params);

    assertTrue(result.isApplied());
    assertEquals("body {}\n", Files.readString(projectRoot.resolve("styles/custom.css")));
  }

  @Test
  @DisplayName("rejects a path escaping the project root")
  void shouldRejectEscapingPath() {
    JsonObject params = replaceParams("body {}\n", StylesheetModifier.version(null));
    params.addProperty("file", "../outside.css");

    assertThrows(CraftforjActionException.class, () -> action.handle(params));
  }

  @Test
  @DisplayName("rejects missing parameters")
  void shouldRejectMissingParams() {
    assertThrows(CraftforjActionException.class, () -> action.handle(null));
  }

  @Test
  @DisplayName("treats JSON null values as missing")
  void shouldTreatJsonNullAsMissing() throws IOException {
    JsonObject params = new JsonObject();
    params.add("file", JsonNull.INSTANCE);
    params.add("changes", JsonNull.INSTANCE);
    params.add("baseVersion", JsonNull.INSTANCE);
    params.add("dryRun", JsonNull.INSTANCE);

    StylesheetResult result = action.handle(params);

    assertFalse(result.isApplied());
    assertTrue(result.getError().contains("Nothing to change"));
    assertEquals("body {\n  margin: 0;\n}\n", Files.readString(stylesheet));
  }

  @Test
  @DisplayName("rejects a blank base version")
  void shouldRejectBlankBaseVersion() {
    assertThrows(CraftforjActionException.class,
        () -> action.handle(replaceParams("body {}\n", " ")));
  }

  @Test
  @DisplayName("writes an END region after the app's own CSS")
  void shouldWriteRegionAtEnd() throws IOException {
    StylesheetResult result =
        action.handle(changeParams(regionChange("theme", ":root { --x: 1; }", "END")));

    assertTrue(result.isApplied());
    String content = Files.readString(stylesheet);
    assertTrue(
        content.indexOf("body {\n  margin: 0;\n}") < content
            .indexOf(StylesheetRegions.open("theme")),
        "the app's own CSS stays ahead of an END region");
    assertEquals(":root { --x: 1; }", StylesheetRegions.find(content, "theme"));
  }

  @Test
  @DisplayName("keeps a region below a charset when no placement is given")
  void shouldDefaultRegionPlacementToStart() throws IOException {
    Files.writeString(stylesheet, "@charset \"utf-8\";\nbody {}\n");

    action.handle(changeParams(regionChange("theme", ":root { --x: 1; }")));

    String content = Files.readString(stylesheet);
    assertTrue(content.startsWith("@charset \"utf-8\";"));
    assertTrue(content.indexOf(StylesheetRegions.open("theme")) < content.indexOf("body {}"),
        "the default placement still puts the region ahead of the app's rules");
  }

  @Test
  @DisplayName("replaces an existing region in place even when END is asked for")
  void shouldReplaceStartRegionInPlaceWhenEndAsked() throws IOException {
    action.handle(changeParams(regionChange("theme", ":root { --x: 1; }", "START")));
    StylesheetResult result =
        action.handle(changeParams(regionChange("theme", ":root { --x: 2; }", "END")));

    assertTrue(result.isApplied());
    String content = Files.readString(stylesheet);
    assertTrue(content.startsWith(StylesheetRegions.open("theme")),
        "a region already at the top stays there, regardless of the placement asked for");
    assertEquals(":root { --x: 2; }", StylesheetRegions.find(content, "theme"));
  }

  @Test
  @DisplayName("replaces an existing region in place even when START is asked for")
  void shouldReplaceEndRegionInPlaceWhenStartAsked() throws IOException {
    action.handle(changeParams(regionChange("theme", ":root { --x: 1; }", "END")));
    StylesheetResult result =
        action.handle(changeParams(regionChange("theme", ":root { --x: 2; }", "START")));

    assertTrue(result.isApplied());
    String content = Files.readString(stylesheet);
    assertTrue(content.indexOf("body {\n  margin: 0;\n}") < content
        .indexOf(StylesheetRegions.open("theme")), "a region already at the end stays there");
    assertEquals(":root { --x: 2; }", StylesheetRegions.find(content, "theme"));
  }

  @Test
  @DisplayName("writes a START and an END region in one atomic call")
  void shouldWriteStartAndEndRegionsTogether() throws IOException {
    StylesheetResult result =
        action.handle(changeParams(regionChange("top", ":root { --top: 1; }", "START"),
            regionChange("bottom", ":root { --bottom: 1; }", "END")));

    assertTrue(result.isApplied());
    String content = Files.readString(stylesheet);
    assertTrue(content.startsWith(StylesheetRegions.open("top")),
        "the START region opens the file");
    assertTrue(content.indexOf(StylesheetRegions.open("bottom")) > content
        .indexOf("body {\n  margin: 0;\n}"), "the END region trails the app's own CSS");
    assertEquals(":root { --top: 1; }", StylesheetRegions.find(content, "top"));
    assertEquals(":root { --bottom: 1; }", StylesheetRegions.find(content, "bottom"));
  }

  @Test
  @DisplayName("writes an END region into an empty stylesheet")
  void shouldWriteEndRegionIntoEmptyFile() throws IOException {
    Files.writeString(stylesheet, "");

    StylesheetResult result =
        action.handle(changeParams(regionChange("theme", ":root { --x: 1; }", "END")));

    assertTrue(result.isApplied());
    assertEquals(":root { --x: 1; }",
        StylesheetRegions.find(Files.readString(stylesheet), "theme"));
  }

  @Test
  @DisplayName("removes an END region and leaves the app's CSS in place")
  void shouldRemoveEndRegion() throws IOException {
    action.handle(changeParams(regionChange("theme", ":root { --x: 1; }", "END")));
    StylesheetResult result = action.handle(changeParams(regionChange("theme", "", "END")));

    assertTrue(result.isApplied());
    String content = Files.readString(stylesheet);
    assertEquals(null, StylesheetRegions.find(content, "theme"));
    assertFalse(content.contains("webforj-devtools:theme"));
    assertTrue(content.contains("body {\n  margin: 0;\n}"));
  }
}
