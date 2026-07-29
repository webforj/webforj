package com.webforj.devtools.craftforj.styles;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.devtools.craftforj.styles.model.StylesheetChange;
import com.webforj.devtools.craftforj.styles.model.StylesheetResult;
import com.webforj.devtools.craftforj.styles.model.StylesheetWriteResult;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class StylesheetModifierTest {

  @TempDir
  Path tempDir;

  private final StylesheetModifier modifier = new StylesheetModifier();
  private Path stylesheet;

  @BeforeEach
  void createStylesheet() throws IOException {
    stylesheet = tempDir.resolve("app.css");
    Files.writeString(stylesheet, "body {\n  margin: 0;\n}\n");
  }

  private static StylesheetChange edit(String oldText, String newText) {
    StylesheetChange change = new StylesheetChange();
    change.setType(StylesheetChange.Type.EDIT);
    change.setOldText(oldText);
    change.setNewText(newText);

    return change;
  }

  private static StylesheetChange prepend(String text) {
    StylesheetChange change = new StylesheetChange();
    change.setType(StylesheetChange.Type.PREPEND);
    change.setText(text);

    return change;
  }

  private static StylesheetChange append(String text) {
    StylesheetChange change = new StylesheetChange();
    change.setType(StylesheetChange.Type.APPEND);
    change.setText(text);

    return change;
  }

  @Nested
  @DisplayName("read")
  class Read {

    @Test
    @DisplayName("Should return content of an existing file")
    void shouldReturnContent() {
      assertEquals("body {\n  margin: 0;\n}\n", modifier.read(stylesheet));
    }

    @Test
    @DisplayName("Should return null for a missing file")
    void shouldReturnNullForMissingFile() {
      assertNull(modifier.read(tempDir.resolve("missing.css")));
    }
  }

  @Nested
  @DisplayName("apply")
  class Apply {

    @Test
    @DisplayName("Should replace a unique match and write the file")
    void shouldReplaceUniqueMatch() throws IOException {
      StylesheetResult result =
          modifier.apply(stylesheet, List.of(edit("margin: 0;", "margin: 1rem;")));

      assertNull(result.getError());
      assertTrue(result.isApplied());
      assertEquals("body {\n  margin: 1rem;\n}\n", Files.readString(stylesheet));
    }

    @Test
    @DisplayName("Should append a block with newline separation")
    void shouldAppendBlock() throws IOException {
      StylesheetResult result =
          modifier.apply(stylesheet, List.of(append("dwc-button {\n  --dwc-font-size: 1rem;\n}")));

      assertNull(result.getError());
      assertEquals("body {\n  margin: 0;\n}\ndwc-button {\n  --dwc-font-size: 1rem;\n}\n",
          Files.readString(stylesheet));
    }

    @Test
    @DisplayName("Should apply changes in list order")
    void shouldApplyChangesInOrder() throws IOException {
      StylesheetResult result = modifier.apply(stylesheet,
          List.of(edit("margin: 0;", "margin: 1rem;"),
              prepend("@import url('https://fonts.example/a.css');"),
              append(":root {\n  --dwc-font-family: serif;\n}")));

      assertNull(result.getError());
      assertEquals("@import url('https://fonts.example/a.css');\n" + "body {\n  margin: 1rem;\n}\n"
          + ":root {\n  --dwc-font-family: serif;\n}\n", Files.readString(stylesheet));
    }

    @Test
    @DisplayName("Should fail when the text is not found")
    void shouldFailWhenTextNotFound() throws IOException {
      StylesheetResult result =
          modifier.apply(stylesheet, List.of(edit("padding: 0;", "padding: 1rem;")));

      assertFalse(result.isApplied());
      assertTrue(result.getError().contains("not found"));
      assertEquals("body {\n  margin: 0;\n}\n", Files.readString(stylesheet));
    }

    @Test
    @DisplayName("Should fail when the text is ambiguous")
    void shouldFailWhenTextAmbiguous() throws IOException {
      Files.writeString(stylesheet, "a { color: red; }\nb { color: red; }\n");

      StylesheetResult result =
          modifier.apply(stylesheet, List.of(edit("color: red;", "color: blue;")));

      assertFalse(result.isApplied());
      assertTrue(result.getError().contains("2 times"));
    }

    @Test
    @DisplayName("Should fail without writing when a later change is invalid")
    void shouldNotWritePartially() throws IOException {
      StylesheetResult result = modifier.apply(stylesheet,
          List.of(edit("margin: 0;", "margin: 1rem;"), edit("nope", "never")));

      assertFalse(result.isApplied());
      assertEquals("body {\n  margin: 0;\n}\n", Files.readString(stylesheet));
    }

    @Test
    @DisplayName("Should fail for a missing stylesheet")
    void shouldFailForMissingStylesheet() {
      StylesheetResult result =
          modifier.apply(tempDir.resolve("missing.css"), List.of(append("a {}")));

      assertFalse(result.isApplied());
      assertTrue(result.getError().contains("not found"));
    }

    @Test
    @DisplayName("Should fail when there are no changes")
    void shouldFailWhenNothingToChange() {
      StylesheetResult result = modifier.apply(stylesheet, List.of());

      assertFalse(result.isApplied());
      assertTrue(result.getError().contains("Nothing to change"));
    }

    @Test
    @DisplayName("Should reject a change without a type")
    void shouldRejectMissingType() {
      StylesheetChange change = new StylesheetChange();
      change.setText("a {}");

      StylesheetResult result = modifier.apply(stylesheet, List.of(change));

      assertFalse(result.isApplied());
      assertTrue(result.getError().contains("missing its type"));
    }

    @Test
    @DisplayName("Should reject an edit with empty oldText")
    void shouldRejectEmptyOldText() {
      StylesheetResult result = modifier.apply(stylesheet, List.of(edit("", "x")));

      assertFalse(result.isApplied());
      assertTrue(result.getError().contains("oldText is empty"));
    }

    @Test
    @DisplayName("Should reject an addition without text")
    void shouldRejectAdditionWithoutText() {
      StylesheetResult result = modifier.apply(stylesheet, List.of(append("  ")));

      assertFalse(result.isApplied());
      assertTrue(result.getError().contains("needs text"));
    }

    @Test
    @DisplayName("Should reject appending a block whose prelude already exists at top level")
    void shouldRejectDuplicateTopLevelBlock() throws IOException {
      Files.writeString(stylesheet, ":root {\n  --dwc-color-primary-seed: #800080;\n}\n");

      StylesheetResult result =
          modifier.apply(stylesheet, List.of(append(":root {\n  --dwc-font-family: serif;\n}")));

      assertFalse(result.isApplied());
      assertTrue(result.getError().contains(":root"));
      assertTrue(result.getError().contains("already exists"));
      assertEquals(":root {\n  --dwc-color-primary-seed: #800080;\n}\n",
          Files.readString(stylesheet));
    }

    @Test
    @DisplayName("Should allow appending a block with a new prelude")
    void shouldAllowNewPreludeAppend() throws IOException {
      Files.writeString(stylesheet, ":root {\n  --dwc-font-size: 1rem;\n}\n");

      StylesheetResult result = modifier.apply(stylesheet,
          List.of(append("dwc-button[dwc-id=\"5\"] {\n  --dwc-font-size: 2rem;\n}")));

      assertTrue(result.isApplied());
      assertTrue(Files.readString(stylesheet).contains("dwc-button[dwc-id=\"5\"]"));
    }

    @Test
    @DisplayName("Should reject appending text already in the stylesheet")
    void shouldRejectAppendAlreadyPresent() throws IOException {
      StylesheetResult result =
          modifier.apply(stylesheet, List.of(append("body {\n  margin: 0;\n}")));

      assertFalse(result.isApplied());
      assertTrue(result.getError().contains("already in the stylesheet"));
    }

    @Test
    @DisplayName("Should reject appending a duplicate at-rule block and ignore nested selectors")
    void shouldRejectDuplicateMediaBlock() throws IOException {
      Files.writeString(stylesheet,
          "@media (max-width: 600px) {\n  .sidebar {\n    display: none;\n  }\n}\n");

      StylesheetResult duplicate = modifier.apply(stylesheet,
          List.of(append("@media (max-width: 600px) {\n  .footer {\n    display: none;\n  }\n}")));

      assertFalse(duplicate.isApplied());
      assertTrue(duplicate.getError().contains("@media (max-width: 600px)"));

      StylesheetResult nested =
          modifier.apply(stylesheet, List.of(append(".sidebar {\n  color: red;\n}")));

      assertTrue(nested.isApplied());
    }

    @Test
    @DisplayName("Should ignore braces inside strings and comments when detecting blocks")
    void shouldIgnoreBracesInStringsAndComments() throws IOException {
      Files.writeString(stylesheet, "a::before {\n  content: \"}\";\n}\n/* body { */\n");

      StylesheetResult body =
          modifier.apply(stylesheet, List.of(append("body {\n  margin: 0;\n}")));

      assertTrue(body.isApplied());

      StylesheetResult duplicate =
          modifier.apply(stylesheet, List.of(append("a::before {\n  content: \"x\";\n}")));

      assertFalse(duplicate.isApplied());
      assertTrue(duplicate.getError().contains("a::before"));
    }

    @Test
    @DisplayName("Should prepend text at the top of the file")
    void shouldPrependText() throws IOException {
      StylesheetResult result = modifier.apply(stylesheet, List.of(prepend(
          "@import url('https://fonts.googleapis.com/css2?family=Orbitron&display=swap');")));

      assertNull(result.getError());
      assertTrue(result.isApplied());
      assertEquals(
          "@import url('https://fonts.googleapis.com/css2?family=Orbitron&display=swap');\n"
              + "body {\n  margin: 0;\n}\n",
          Files.readString(stylesheet));
    }

    @Test
    @DisplayName("Should reject prepending text already in the stylesheet")
    void shouldRejectDuplicatePrepend() throws IOException {
      String importRule = "@import url('https://fonts.googleapis.com/css2?family=Orbitron');";
      Files.writeString(stylesheet, importRule + "\nbody {}\n");

      StylesheetResult result = modifier.apply(stylesheet, List.of(prepend(importRule)));

      assertFalse(result.isApplied());
      assertTrue(result.getError().contains("already in the stylesheet"));
    }
  }

  @Nested
  @DisplayName("parser edge cases")
  class ParserEdgeCases {

    @Test
    @DisplayName("Should survive an unterminated comment")
    void shouldSurviveUnterminatedComment() throws IOException {
      Files.writeString(stylesheet, "body {\n  margin: 0;\n}\n/* unterminated");

      StylesheetResult result = modifier.apply(stylesheet, List.of(append(".x { color: red; }")));

      assertNull(result.getError());
      assertTrue(result.isApplied());
      assertTrue(Files.readString(stylesheet).contains(".x { color: red; }"));
    }

    @Test
    @DisplayName("Should survive an unterminated string")
    void shouldSurviveUnterminatedString() throws IOException {
      Files.writeString(stylesheet, ".a::before {\n  content: \"unterminated;\n}\n");

      StylesheetResult result = modifier.apply(stylesheet, List.of(append(".x { color: red; }")));

      assertNull(result.getError());
      assertTrue(result.isApplied());
    }

    @Test
    @DisplayName("Should skip escaped quotes inside strings")
    void shouldSkipEscapedQuotes() throws IOException {
      Files.writeString(stylesheet, ".a::before {\n  content: \"a\\\"{b\";\n}\n");

      StylesheetResult result = modifier.apply(stylesheet, List.of(append(".a { color: red; }")));

      // The brace inside the escaped string must not open a fake top-level block
      assertNull(result.getError());
      assertTrue(result.isApplied());
    }

    @Test
    @DisplayName("Should prepend after a leading @charset rule")
    void shouldPrependAfterCharset() throws IOException {
      Files.writeString(stylesheet, "@charset \"UTF-8\";\nbody {\n  margin: 0;\n}\n");

      StylesheetResult result =
          modifier.apply(stylesheet, List.of(prepend("@import url('https://fonts.example/a');")));

      assertNull(result.getError());
      assertEquals(
          "@charset \"UTF-8\";\n@import url('https://fonts.example/a');\nbody {\n  margin: 0;\n}\n",
          Files.readString(stylesheet));
    }
  }

  @Nested
  @DisplayName("preview")
  class Preview {

    @Test
    @DisplayName("Should compute the result without writing")
    void shouldComputeWithoutWriting() throws IOException {
      StylesheetResult result =
          modifier.preview(stylesheet, List.of(edit("margin: 0;", "margin: 2rem;")));

      assertNull(result.getError());
      assertFalse(result.isApplied());
      assertEquals("body {\n  margin: 2rem;\n}\n", result.getContent());
      assertEquals("body {\n  margin: 0;\n}\n", Files.readString(stylesheet));
    }
  }

  @Nested
  @DisplayName("version")
  class Version {

    @Test
    @DisplayName("Should version equal content equally")
    void shouldVersionEqualContentEqually() {
      assertEquals(StylesheetModifier.version("body {}"), StylesheetModifier.version("body {}"));
    }

    @Test
    @DisplayName("Should version different content differently")
    void shouldVersionDifferentContentDifferently() {
      assertNotEquals(StylesheetModifier.version("body {}"), StylesheetModifier.version("a {}"));
    }

    @Test
    @DisplayName("Should version a missing file like the empty string")
    void shouldVersionMissingFileLikeEmpty() {
      assertEquals(StylesheetModifier.version(""), StylesheetModifier.version(null));
    }
  }

  @Nested
  @DisplayName("replace")
  class Replace {

    @Test
    @DisplayName("Should write when the base version matches")
    void shouldWriteOnMatchingVersion() throws IOException {
      String base = Files.readString(stylesheet);

      StylesheetWriteResult result =
          modifier.replace(stylesheet, "a {}\n", StylesheetModifier.version(base));

      assertTrue(result.isApplied());
      assertFalse(result.isConflict());
      assertEquals(StylesheetModifier.version("a {}\n"), result.getVersion());
      assertEquals("a {}\n", Files.readString(stylesheet));
    }

    @Test
    @DisplayName("Should reject a stale base version without writing")
    void shouldRejectStaleVersion() throws IOException {
      String original = Files.readString(stylesheet);

      StylesheetWriteResult result =
          modifier.replace(stylesheet, "a {}\n", StylesheetModifier.version("something else"));

      assertFalse(result.isApplied());
      assertTrue(result.isConflict());
      assertEquals(original, result.getContent());
      assertEquals(StylesheetModifier.version(original), result.getVersion());
      assertEquals(original, Files.readString(stylesheet));
    }

    @Test
    @DisplayName("Should create the file and its directories from the missing file version")
    void shouldCreateFileAndDirectories() throws IOException {
      Path nested = tempDir.resolve("src/main/frontend/app.css");

      StylesheetWriteResult result =
          modifier.replace(nested, "a {}\n", StylesheetModifier.version(null));

      assertTrue(result.isApplied());
      assertEquals("a {}\n", Files.readString(nested));
    }

    @Test
    @DisplayName("Should leave no temporary files behind")
    void shouldLeaveNoTemporaryFiles() throws IOException {
      String base = Files.readString(stylesheet);
      modifier.replace(stylesheet, "a {}\n", StylesheetModifier.version(base));

      try (var files = Files.list(tempDir)) {
        assertEquals(List.of(stylesheet), files.toList());
      }
    }
  }
}
