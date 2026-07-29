package com.webforj.devtools.craftforj.docs.builder;

import com.webforj.devtools.craftforj.docs.index.HtmlElements;
import com.webforj.devtools.craftforj.docs.model.DocsEntry;
import com.webforj.devtools.craftforj.docs.model.DwcStylingData;

/**
 * Builds markdown documentation from docs entry and DWC styling data.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class MarkdownBuilder {

  private static final String FRONTMATTER_DELIMITER = "---\n";
  private static final String COLLAPSIBLE_END = ":::\n\n";

  private MarkdownBuilder() {}

  /**
   * Builds complete markdown from a docs entry and optional DWC styling data.
   *
   * @param entry the docs entry from the index
   * @param styling the DWC styling data (may be null)
   * @return the complete markdown string
   */
  public static String build(DocsEntry entry, DwcStylingData styling) {
    StringBuilder md = new StringBuilder();
    String clientTag = entry.getClientComponent();
    boolean isHtmlElement = HtmlElements.isHtmlElement(clientTag);

    // Build frontmatter
    md.append(FRONTMATTER_DELIMITER);
    if (isHtmlElement) {
      md.append("title: <").append(clientTag).append(">\n");
      md.append("mdn: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/").append(clientTag)
          .append("\n");
    } else {
      md.append("title: ").append(entry.getTitle()).append("\n");
    }
    if (entry.getSince() != null) {
      md.append("since: ").append(entry.getSince()).append("\n");
    }
    if (entry.getJavadoc() != null) {
      md.append("javadoc: ").append(entry.getJavadoc()).append("\n");
    }
    if (entry.getDocs() != null) {
      md.append("docs: ").append(entry.getDocs()).append("\n");
    }
    md.append(FRONTMATTER_DELIMITER).append("\n");

    // Add content or HTML element description
    if (entry.getContent() != null && !entry.getContent().isEmpty()) {
      md.append(entry.getContent()).append("\n\n");
    } else if (isHtmlElement) {
      md.append("Standard HTML `<").append(clientTag).append(
          ">` element. See <a href=\"https://developer.mozilla.org/en-US/docs/Web/HTML/Element/")
          .append(clientTag).append("\" target=\"_blank\">MDN documentation</a> for details.\n\n");
    }

    // Add DWC styling sections
    if (styling != null && styling.hasData()) {
      appendStylingData(md, styling);
    }

    return md.toString().trim();
  }

  /**
   * Builds markdown for DWC styling data only (no docs entry).
   *
   * @param tagName the component tag name
   * @param styling the DWC styling data
   * @return the markdown string
   */
  public static String buildStylingOnly(String tagName, DwcStylingData styling) {
    StringBuilder md = new StringBuilder();

    // Build frontmatter
    md.append(FRONTMATTER_DELIMITER);
    md.append("title: ").append(tagName).append("\n");
    md.append(FRONTMATTER_DELIMITER).append("\n");

    // Add styling sections
    if (styling != null && styling.hasData()) {
      appendStylingData(md, styling);
    }

    return md.toString().trim();
  }

  /**
   * Builds markdown for a standard HTML element.
   *
   * @param tagName the HTML element tag name
   * @return the markdown string
   */
  public static String buildHtmlElement(String tagName) {
    StringBuilder md = new StringBuilder();

    md.append(FRONTMATTER_DELIMITER);
    md.append("title: <").append(tagName).append(">\n");
    md.append("mdn: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/").append(tagName)
        .append("\n");
    md.append(FRONTMATTER_DELIMITER).append("\n");
    md.append("Standard HTML `<").append(tagName)
        .append(
            ">` element. See <a href=\"https://developer.mozilla.org/en-US/docs/Web/HTML/Element/")
        .append(tagName).append("\" target=\"_blank\">MDN documentation</a> for details.");

    return md.toString();
  }

  private static void appendStylingData(StringBuilder md, DwcStylingData styling) {
    // Shadow Parts
    if (styling.getParts() != null && !styling.getParts().isEmpty()) {
      md.append("::: collapsible Shadow Parts\n");
      md.append("| Part | Description |\n");
      md.append("|------|-------------|\n");
      for (DwcStylingData.Part part : styling.getParts()) {
        md.append("| `").append(escapeCell(part.getName())).append("` | ");
        md.append(escapeCell(part.getDescription())).append(" |\n");
      }
      md.append(COLLAPSIBLE_END);
    }

    // CSS Properties
    if (styling.getCssProperties() != null && !styling.getCssProperties().isEmpty()) {
      md.append("::: collapsible CSS Properties\n");
      md.append("| Property | Description |\n");
      md.append("|----------|-------------|\n");
      for (DwcStylingData.CssProperty prop : styling.getCssProperties()) {
        md.append("| `").append(escapeCell(prop.getName())).append("` | ");
        md.append(escapeCell(prop.getDescription())).append(" |\n");
      }
      md.append(COLLAPSIBLE_END);
    }

    // Slots
    if (styling.getSlots() != null && !styling.getSlots().isEmpty()) {
      md.append("::: collapsible Slots\n");
      md.append("| Slot | Description |\n");
      md.append("|------|-------------|\n");
      for (DwcStylingData.Slot slot : styling.getSlots()) {
        String name =
            slot.getName() == null || slot.getName().isEmpty() ? "(default)" : slot.getName();
        md.append("| `").append(escapeCell(name)).append("` | ");
        md.append(escapeCell(slot.getDescription())).append(" |\n");
      }
      md.append(COLLAPSIBLE_END);
    }

    // Reflected Attributes
    if (styling.getReflects() != null && !styling.getReflects().isEmpty()) {
      md.append("::: collapsible Reflected Attributes\n");
      md.append("| Attribute | Type | Description |\n");
      md.append("|-----------|------|-------------|\n");
      for (DwcStylingData.ReflectedAttribute attr : styling.getReflects()) {
        md.append("| `").append(escapeCell(attr.getName())).append("` | ");
        md.append(escapeCell(attr.getType())).append(" | ");
        md.append(escapeCell(attr.getDescription())).append(" |\n");
      }
      md.append(COLLAPSIBLE_END);
    }

    // Dependencies
    if (styling.getDependencies() != null && !styling.getDependencies().isEmpty()) {
      md.append("::: collapsible Dependencies\n");
      for (String dep : styling.getDependencies()) {
        md.append("- `").append(dep).append("`\n");
      }
      md.append(COLLAPSIBLE_END);
    }
  }

  private static String escapeCell(String value) {
    if (value == null) {
      return "";
    }
    return value.replace("|", "\\|").replace("\n", " ");
  }
}
