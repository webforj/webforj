package com.webforj.utilities;

import com.typesafe.config.Config;
import com.webforj.Environment;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import org.jsoup.Jsoup;
import org.jsoup.nodes.TextNode;

/**
 * Converts between a component's plain text and the HTML content that backs it.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class HtmlText {
  static final String LEGACY_HTML_IN_TEXT = "webforj.legacyHtmlInText";
  private static final String HTML_MARKER = "<html>";
  private static final Logger logger = System.getLogger(HtmlText.class.getName());
  private static final Set<String> warnedCallers = ConcurrentHashMap.newKeySet();

  private HtmlText() {}

  /**
   * Checks whether the given value opts in to HTML rendering by being wrapped in an {@code <html>}
   * tag.
   *
   * @param text the value to check
   * @return {@code true} when the trimmed value starts with {@code <html>}
   */
  public static boolean isWrappedWithHtmlTag(String text) {
    return (text == null ? "" : text).trim().startsWith(HTML_MARKER);
  }

  /**
   * Converts plain text into the value to store on a sink that renders through {@code innerHTML},
   * so the text is shown literally.
   *
   * <p>
   * The reserved HTML characters are encoded, so a value such as {@code "<b>hi</b>"} becomes
   * {@code "&lt;b&gt;hi&lt;/b&gt;"} and is shown as those characters rather than run as markup. A
   * value wrapped in {@code <html>} is the deprecated opt-in described on
   * {@link #LEGACY_HTML_IN_TEXT}. A {@code null} value is treated as an empty string.
   * </p>
   *
   * @param text the plain text to convert
   * @return the backing content, never {@code null}
   */
  public static String forHtmlSink(String text) {
    if (text == null || text.isEmpty()) {
      return "";
    }

    if (!isWrappedWithHtmlTag(text)) {
      return escape(text);
    }

    if (isLegacyHtmlInTextEnabled()) {
      warnLegacyHtmlInText(text);

      return text;
    }

    return escape(stripMarker(text));
  }

  /**
   * Removes the {@code <html>} opt-in marker, returning the content it wrapped.
   *
   * @param text the value wrapped in {@code <html>}
   * @return the content inside the marker
   */
  public static String stripMarker(String text) {
    String value = (text == null ? "" : text).trim();

    if (value.startsWith(HTML_MARKER)) {
      value = value.substring(HTML_MARKER.length());
    }

    if (value.endsWith("</html>")) {
      value = value.substring(0, value.length() - "</html>".length());
    }

    return value;
  }

  /**
   * Returns the visible text of the given backing content with tags removed and entities decoded.
   *
   * <p>
   * A value produced by {@link #forHtmlSink(String)} is decoded back to its original text. Real
   * markup yields its text content. A {@code null} value is treated as an empty string.
   * </p>
   *
   * @param html the backing content to read
   * @return the visible text, never {@code null}
   */
  public static String toText(String html) {
    if (html == null || html.isEmpty()) {
      return "";
    }

    return Jsoup.parse(html).text();
  }

  private static String escape(String text) {
    return new TextNode(text).outerHtml();
  }

  /**
   * Indicates whether the deprecated {@code <html>} opt-in described on
   * {@link #LEGACY_HTML_IN_TEXT} is enabled.
   *
   * @return {@code true} when {@code <html>}-wrapped text is rendered as markup
   */
  public static boolean isLegacyHtmlInTextEnabled() {
    Environment environment = Environment.getCurrent();
    if (environment == null) {
      return true;
    }

    Config config = environment.getConfig();
    if (config != null && config.hasPath(LEGACY_HTML_IN_TEXT)
        && !config.getIsNull(LEGACY_HTML_IN_TEXT)) {
      return config.getBoolean(LEGACY_HTML_IN_TEXT);
    }

    return true;
  }

  private static void warnLegacyHtmlInText(String text) {
    String origin = findOrigin();
    if (!warnedCallers.add(origin)) {
      return;
    }

    logger.log(Level.WARNING,
        "Passing <html>-wrapped content to setText is deprecated and will be removed in webforJ 27."
            + " Use setHtml for intentional HTML, or set webforj.legacyHtmlInText=false to treat it"
            + " as plain text. Triggered by " + origin + " with value: \"" + snippet(text) + "\".");
  }

  private static String findOrigin() {
    StackTraceElement component = null;
    for (StackTraceElement frame : Thread.currentThread().getStackTrace()) {
      String className = frame.getClassName();
      if (className.equals(HtmlText.class.getName()) || className.startsWith("java.")
          || className.startsWith("jdk.")) {
        continue;
      }

      if (className.startsWith("com.webforj.")) {
        component = frame;
        continue;
      }

      String origin = component == null ? "" : component.getClassName() + " at ";

      return origin + frame;
    }

    return "an unknown location";
  }

  private static String snippet(String text) {
    String value = text.strip();

    return value.length() <= 60 ? value : value.substring(0, 57) + "...";
  }
}
