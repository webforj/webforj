package com.webforj.devtools.craftforj.docs.index;

import java.util.Set;

/**
 * Index for standard HTML elements.
 *
 * <p>
 * This class provides a way to check if a tag name is a standard HTML element.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class HtmlElements {

  private static final Set<String> ELEMENTS = Set.of("a", "abbr", "address", "area", "article",
      "aside", "audio", "b", "base", "bdi", "bdo", "blockquote", "body", "br", "button", "canvas",
      "caption", "cite", "code", "col", "colgroup", "data", "datalist", "dd", "del", "details",
      "dfn", "dialog", "div", "dl", "dt", "em", "embed", "fieldset", "figcaption", "figure",
      "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header", "hgroup", "hr",
      "html", "i", "iframe", "img", "input", "ins", "kbd", "label", "legend", "li", "link", "main",
      "map", "mark", "menu", "meta", "meter", "nav", "noscript", "object", "ol", "optgroup",
      "option", "output", "p", "picture", "pre", "progress", "q", "rp", "rt", "ruby", "s", "samp",
      "script", "search", "section", "select", "slot", "small", "source", "span", "strong", "style",
      "sub", "summary", "sup", "table", "tbody", "td", "template", "textarea", "tfoot", "th",
      "thead", "time", "title", "tr", "track", "u", "ul", "var", "video", "wbr");

  private HtmlElements() {}

  /**
   * Checks if the given tag name is a standard HTML element.
   *
   * @param tagName the tag name to check
   * @return true if it's a standard HTML element
   */
  public static boolean isHtmlElement(String tagName) {
    if (tagName == null || tagName.isEmpty()) {
      return false;
    }
    return ELEMENTS.contains(tagName.toLowerCase());
  }
}
