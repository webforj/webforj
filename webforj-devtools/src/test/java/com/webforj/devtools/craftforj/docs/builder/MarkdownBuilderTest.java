package com.webforj.devtools.craftforj.docs.builder;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.devtools.craftforj.docs.model.DocsEntry;
import com.webforj.devtools.craftforj.docs.model.DwcStylingData;
import java.util.List;
import org.junit.jupiter.api.Test;

class MarkdownBuilderTest {

  @Test
  void shouldBuildMarkdownWithFrontmatter() {
    DocsEntry entry = createEntry("Button", "23.02", "com/webforj/Button", "http://docs",
        "dwc-button", "Test content");
    String md = MarkdownBuilder.build(entry, null);

    assertNotNull(md);
    assertTrue(md.contains("---"));
    assertTrue(md.contains("title: Button"));
    assertTrue(md.contains("since: 23.02"));
    assertTrue(md.contains("javadoc: com/webforj/Button"));
  }

  @Test
  void shouldBuildMarkdownWithContent() {
    DocsEntry entry = createEntry("Button", null, null, null, null, "Test content");
    String md = MarkdownBuilder.build(entry, null);

    assertTrue(md.contains("Test content"));
  }

  @Test
  void shouldBuildMarkdownWithStylingData() {
    DocsEntry entry = createEntry("Button", null, null, null, "dwc-button", "Test content");
    DwcStylingData styling = createStylingData();

    String md = MarkdownBuilder.build(entry, styling);

    assertTrue(md.contains("Shadow Parts"));
    assertTrue(md.contains("control"));
    assertTrue(md.contains("CSS Properties"));
    assertTrue(md.contains("--dwc-button-color"));
  }

  @Test
  void shouldBuildStylingOnlyMarkdown() {
    DwcStylingData styling = createStylingData();
    String md = MarkdownBuilder.buildStylingOnly("dwc-button", styling);

    assertTrue(md.contains("title: dwc-button"));
    assertTrue(md.contains("Shadow Parts"));
  }

  @Test
  void shouldBuildHtmlElementMarkdown() {
    String md = MarkdownBuilder.buildHtmlElement("div");

    assertTrue(md.contains("title: <div>"));
    assertTrue(md.contains("mdn:"));
    assertTrue(md.contains("developer.mozilla.org"));
    assertTrue(md.contains("Standard HTML"));
  }

  @Test
  void shouldBuildHtmlElementFromDocsEntry() {
    DocsEntry entry = createEntry("H3", null, null, null, "h3", null);
    String md = MarkdownBuilder.build(entry, null);

    assertNotNull(md);
    assertTrue(md.contains("title: <h3>"));
    assertTrue(md.contains("mdn:"));
    assertTrue(md.contains("developer.mozilla.org"));
    assertTrue(md.contains("Standard HTML"));
  }

  @Test
  void shouldEscapeTableCells() {
    DwcStylingData.Part part = new DwcStylingData.Part("test|part", "Description with | pipe");
    DwcStylingData styling = new DwcStylingData(List.of(part), null, null, null, null);

    DocsEntry entry = createEntry("Test", null, null, null, "dwc-test", "Test content");
    String md = MarkdownBuilder.build(entry, styling);

    // Pipes should be escaped
    assertFalse(md.contains("| test|part |"));
    assertTrue(md.contains("test\\|part"));
  }

  private DocsEntry createEntry(String title, String since, String javadoc, String docs,
      String clientComponent, String content) {
    DocsEntry entry = new DocsEntry();
    entry.setTitle(title);
    entry.setSince(since);
    entry.setJavadoc(javadoc);
    entry.setDocs(docs);
    entry.setClientComponent(clientComponent);
    entry.setContent(content);
    return entry;
  }

  private DwcStylingData createStylingData() {
    DwcStylingData.Part part = new DwcStylingData.Part("control", "The base wrapper");
    DwcStylingData.CssProperty css =
        new DwcStylingData.CssProperty("--dwc-button-color", "Text color");
    return new DwcStylingData(List.of(part), List.of(css), null, null, null);
  }
}
