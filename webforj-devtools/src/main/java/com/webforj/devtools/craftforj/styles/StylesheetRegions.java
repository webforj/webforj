package com.webforj.devtools.craftforj.styles;

import com.webforj.devtools.craftforj.styles.model.RegionPlacement;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Named regions inside the application stylesheet.
 *
 * <p>
 * A region is a stretch of the file fenced by a pair of CSS comments, replaced whole by its writer.
 * Everything outside a region is changed only by an exact match edit.
 * </p>
 *
 * <pre>
 * &#47;* webforj-devtools:theme - generated, do not edit *&#47;
 * :root { ... }
 * &#47;* /webforj-devtools:theme *&#47;
 * </pre>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class StylesheetRegions {

  /** Pattern a region name must match. */
  public static final Pattern NAME = Pattern.compile("[a-z][a-z0-9-]{0,39}");

  private static final String PREFIX = "webforj-devtools:";
  private static final String NOTE = " - generated, do not edit";

  private StylesheetRegions() {}

  /**
   * The comment opening a region.
   *
   * @param name the region name
   * @return the opening fence
   */
  public static String open(String name) {
    return "/* " + PREFIX + name + NOTE + " */";
  }

  /**
   * The comment closing a region.
   *
   * @param name the region name
   * @return the closing fence
   */
  public static String close(String name) {
    return "/* /" + PREFIX + name + " */";
  }

  /**
   * Validates a region name.
   *
   * @param name the region name
   * @return the reason it is invalid, or {@code null} when the name is usable
   */
  public static String validateName(String name) {
    if (name == null || name.isBlank()) {
      return "A REGION change needs a region name.";
    }

    if (!NAME.matcher(name).matches()) {
      return "Region \"" + name + "\" is not a usable name: lowercase letters, digits and dashes, "
          + "starting with a letter, up to forty characters.";
    }

    return null;
  }

  /**
   * Every region in the content, in the order they appear.
   *
   * @param content the stylesheet content, or {@code null}
   * @return the region bodies by name, empty when the content owns none
   */
  public static Map<String, String> findAll(String content) {
    Map<String, String> regions = new LinkedHashMap<>();
    if (content == null || content.isEmpty()) {
      return regions;
    }

    Matcher matcher = fence().matcher(content);
    while (matcher.find()) {
      regions.put(matcher.group(1), matcher.group(2).strip());
    }

    return regions;
  }

  /**
   * The body of one region.
   *
   * @param content the stylesheet content, or {@code null}
   * @param name the region name
   * @return the body, or {@code null} when the content does not carry that region
   */
  public static String find(String content, String name) {
    return findAll(content).get(name);
  }

  /**
   * Puts a region into the content, replacing it where it already is.
   *
   * <p>
   * A region already in the content is replaced where it stands, whatever the placement asks for.
   * Blank text removes the region and its fences.
   * </p>
   *
   * @param content the stylesheet content
   * @param name the region name
   * @param text the body, or blank to remove the region
   * @param placement where a region the content does not carry yet is put
   * @return the content with the region written
   */
  public static String write(String content, String name, String text, RegionPlacement placement) {
    String body = text == null ? "" : text.strip();
    String existing = content == null ? "" : content;
    Matcher matcher = fence(name).matcher(existing);

    if (matcher.find()) {
      // The fence regex consumes the trailing newline, put back here to avoid losing a line per
      // save.
      String replacement = body.isEmpty() ? "" : Matcher.quoteReplacement(build(name, body) + "\n");
      String written = matcher.replaceFirst(replacement);

      return body.isEmpty() ? tidy(written) : written;
    }

    if (body.isEmpty()) {
      return existing;
    }

    return placement == RegionPlacement.END ? append(existing, build(name, body))
        : prepend(existing, build(name, body));
  }

  private static String append(String content, String region) {
    if (content.isBlank()) {
      return region + "\n";
    }

    return content.stripTrailing() + "\n\n" + region + "\n";
  }

  private static String prepend(String content, String region) {
    if (content.isBlank()) {
      return region + "\n";
    }

    int opening = findOpeningEnd(content);
    if (opening <= 0) {
      return region + "\n\n" + content;
    }

    return content.substring(0, opening) + "\n\n" + region + "\n" + content.substring(opening);
  }

  /**
   * Finds the offset past any leading {@code @charset} or {@code @import} rules.
   *
   * @param content the stylesheet content
   * @return the offset, or zero when none are present
   */
  private static int findOpeningEnd(String content) {
    int offset = 0;

    while (true) {
      int start = skipBlank(content, offset);
      boolean opens = content.startsWith("@charset", start) || content.startsWith("@import", start);
      if (!opens) {
        return offset;
      }

      int end = content.indexOf(';', start);
      if (end < 0) {
        return offset;
      }

      offset = end + 1;
    }
  }

  /**
   * Finds the offset of the next character that is not whitespace or a comment.
   *
   * @param content the stylesheet content
   * @param from the offset to read from
   * @return the offset of the next meaningful character
   */
  private static int skipBlank(String content, int from) {
    int offset = from;

    while (offset < content.length()) {
      if (Character.isWhitespace(content.charAt(offset))) {
        offset++;
      } else if (content.startsWith("/*", offset)) {
        int end = content.indexOf("*/", offset + 2);
        if (end < 0) {
          return content.length();
        }

        offset = end + 2;
      } else {
        return offset;
      }
    }

    return offset;
  }

  private static Pattern fence() {
    return fence("([a-z][a-z0-9-]*)", "\\1");
  }

  private static Pattern fence(String name) {
    String quoted = Pattern.quote(name);
    return fence(quoted, quoted);
  }

  private static Pattern fence(String opening, String closing) {
    return Pattern.compile("/\\*\\s*" + Pattern.quote(PREFIX) + opening + "[^*]*\\*/" + "(.*?)"
        + "/\\*\\s*/" + Pattern.quote(PREFIX) + closing + "\\s*\\*/\\n?", Pattern.DOTALL);
  }

  private static String build(String name, String body) {
    return open(name) + "\n" + body + "\n" + close(name);
  }

  private static String tidy(String content) {
    return content.replaceAll("\n{3,}", "\n\n").stripTrailing() + (content.isBlank() ? "" : "\n");
  }
}
