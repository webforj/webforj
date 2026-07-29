package com.webforj.devtools.craftforj.styles;

import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.styles.model.RegionPlacement;
import com.webforj.devtools.craftforj.styles.model.StylesheetChange;
import com.webforj.devtools.craftforj.styles.model.StylesheetResult;
import com.webforj.devtools.craftforj.styles.model.StylesheetWriteResult;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * Applies an ordered list of changes to the application stylesheet.
 *
 * <p>
 * Changes apply in list order against the in-memory content and the whole list fails atomically. An
 * edit's {@code oldText} must occur exactly once in the current content. Prepended text lands at
 * the top of the file and appended text at the end. Both are rejected when the text is already
 * present, or when it opens a top-level block whose prelude already has one. Previews compute the
 * result without writing.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class StylesheetModifier {

  private static final int SNIPPET_LENGTH = 60;

  /**
   * Reads the stylesheet content.
   *
   * @param file the stylesheet path
   * @return the content, or {@code null} when the file does not exist
   */
  public String read(Path file) {
    if (!Files.isRegularFile(file)) {
      return null;
    }

    try {
      return Files.readString(file, StandardCharsets.UTF_8);
    } catch (IOException e) {
      throw new CraftforjActionException("Failed to read stylesheet: " + file, e);
    }
  }

  /**
   * Computes the version of stylesheet content.
   *
   * <p>
   * A missing file reads as {@code null} content and versions like the empty string.
   * </p>
   *
   * @param content the content, or {@code null} for a missing file
   * @return the version
   */
  public static String version(String content) {
    String normalized = content == null ? "" : content;

    try {
      MessageDigest digest = MessageDigest.getInstance("SHA-256");
      return HexFormat.of().formatHex(digest.digest(normalized.getBytes(StandardCharsets.UTF_8)));
    } catch (NoSuchAlgorithmException e) {
      throw new CraftforjActionException("SHA-256 is not available", e);
    }
  }

  /**
   * Replaces the whole stylesheet content when the file still matches the base version.
   *
   * <p>
   * The write is atomic, via a temporary sibling file moved over the stylesheet. Parent directories
   * are created when needed. When the file changed since the base version nothing is written and
   * the result carries the current content and version instead.
   * </p>
   *
   * @param file the stylesheet path
   * @param content the full new content
   * @param baseVersion the version the caller last read
   * @return the write result
   */
  public StylesheetWriteResult replace(Path file, String content, String baseVersion) {
    StylesheetWriteResult result = new StylesheetWriteResult();
    result.setPath(file.toString());

    String current = read(file);
    String currentVersion = version(current);

    if (!currentVersion.equals(baseVersion)) {
      result.setConflict(true);
      result.setContent(current);
      result.setVersion(currentVersion);

      return result;
    }

    writeAtomic(file, content);
    result.setApplied(true);
    result.setVersion(version(content));

    return result;
  }

  /**
   * Computes the result of the changes without writing.
   *
   * @param file the stylesheet path
   * @param changes the ordered changes
   * @return the preview result
   */
  public StylesheetResult preview(Path file, List<StylesheetChange> changes) {
    return change(file, changes, true);
  }

  /**
   * Applies the changes and writes the stylesheet.
   *
   * @param file the stylesheet path
   * @param changes the ordered changes
   * @return the apply result
   */
  public StylesheetResult apply(Path file, List<StylesheetChange> changes) {
    return change(file, changes, false);
  }

  /**
   * Applies the changes, optionally against the version the caller last read.
   *
   * <p>
   * A base version turns the write into a compare and swap. When the file moved since it was read
   * nothing is written and the result carries the current content and version instead. Without a
   * base version the changes apply to whatever is on disk.
   * </p>
   *
   * @param file the stylesheet path
   * @param changes the ordered changes
   * @param dryRun {@code true} to compute the result without writing
   * @param baseVersion the version the caller last read, or {@code null} to skip the check
   * @return the write result
   */
  public StylesheetResult write(Path file, List<StylesheetChange> changes, boolean dryRun,
      String baseVersion) {
    if (baseVersion != null) {
      String current = version(read(file));
      if (!current.equals(baseVersion)) {
        StylesheetResult result = new StylesheetResult();
        result.setPath(file.toString());
        result.setConflict(true);
        result.setContent(read(file));
        result.setVersion(current);

        return result;
      }
    }

    return change(file, changes, dryRun);
  }

  private StylesheetResult change(Path file, List<StylesheetChange> changes, boolean dryRun) {
    StylesheetResult result = new StylesheetResult();
    result.setPath(file.toString());

    String content = read(file);
    if (content == null && !isCreating(changes)) {
      result.setError("Stylesheet not found: " + file
          + ". Create it, or configure a different stylesheet path in the craftforJ settings.");

      return result;
    }

    if (content == null) {
      content = "";
    }

    if (changes == null || changes.isEmpty()) {
      result.setError("Nothing to change: provide at least one change.");
      return result;
    }

    for (StylesheetChange change : changes) {
      String error;
      StylesheetChange.Type type = change.getType();

      if (type == StylesheetChange.Type.EDIT) {
        error = validateEdit(change, content);
        if (error == null) {
          content = content.replace(change.getOldText(), change.getNewText());
        }
      } else if (type == StylesheetChange.Type.PREPEND) {
        error = validateAddition(change, content);
        if (error == null) {
          content = prepend(content, change.getText().strip());
        }
      } else if (type == StylesheetChange.Type.APPEND) {
        error = validateAddition(change, content);
        if (error == null) {
          String separator = content.isEmpty() || content.endsWith("\n") ? "" : "\n";
          content = content + separator + change.getText().stripTrailing() + "\n";
        }
      } else if (type == StylesheetChange.Type.REGION) {
        error = StylesheetRegions.validateName(change.getRegion());
        if (error == null) {
          RegionPlacement placement =
              change.getPlacement() == null ? RegionPlacement.START : change.getPlacement();
          content =
              StylesheetRegions.write(content, change.getRegion(), change.getText(), placement);
        }
      } else if (type == StylesheetChange.Type.REPLACE) {
        error = change.getText() == null ? "A REPLACE change needs text." : null;
        if (error == null) {
          content = change.getText();
        }
      } else {
        error = "A change is missing its type: expected EDIT, PREPEND, APPEND, REGION or REPLACE.";
      }

      if (error != null) {
        result.setError(error);
        return result;
      }
    }

    result.setContent(content);

    if (!dryRun) {
      writeAtomic(file, content);
      result.setApplied(true);
    }

    result.setVersion(version(content));

    return result;
  }

  private String prepend(String content, String text) {
    // @charset must stay the first rule, so prepended text (like @import) goes right after it
    String leading = content.stripLeading();
    if (leading.startsWith("@charset")) {
      int offset = content.length() - leading.length();
      int end = content.indexOf(';', offset);
      if (end >= 0) {
        return content.substring(0, end + 1) + "\n" + text + content.substring(end + 1);
      }
    }

    return text + "\n" + content;
  }

  private String validateEdit(StylesheetChange change, String content) {
    String oldText = change.getOldText();
    if (oldText == null || oldText.isEmpty()) {
      return "Edit rejected: oldText is empty.";
    }

    if (change.getNewText() == null) {
      return "Edit rejected: newText is missing for \"" + snippet(oldText) + "\".";
    }

    int occurrences = countOccurrences(content, oldText);
    if (occurrences == 0) {
      return "Text not found in stylesheet: \"" + snippet(oldText) + "\".";
    }

    if (occurrences > 1) {
      return "Text occurs " + occurrences + " times in stylesheet, expected exactly once: \""
          + snippet(oldText) + "\".";
    }

    return null;
  }

  private static String validateAddition(StylesheetChange change, String content) {
    String text = change.getText();
    if (text == null || text.isBlank()) {
      return "A " + change.getType() + " change needs text.";
    }

    if (content.contains(text.strip())) {
      return "The added text is already in the stylesheet.";
    }

    Set<String> existing = topLevelPreludes(content);
    for (String prelude : topLevelPreludes(text)) {
      if (existing.contains(prelude)) {
        return "A top-level \"" + prelude + "\" block already exists in the stylesheet. "
            + "Edit the existing block instead of adding another one.";
      }
    }

    return null;
  }

  private static boolean isCreating(List<StylesheetChange> changes) {
    if (changes == null) {
      return false;
    }

    for (StylesheetChange change : changes) {
      StylesheetChange.Type type = change.getType();
      if (type == StylesheetChange.Type.REPLACE || type == StylesheetChange.Type.REGION) {
        return true;
      }
    }

    return false;
  }

  private void writeAtomic(Path file, String content) {
    try {
      Path parent = file.toAbsolutePath().getParent();
      if (parent != null) {
        Files.createDirectories(parent);
      }

      Path temp = Files.createTempFile(parent, file.getFileName().toString(), ".tmp");
      try {
        Files.writeString(temp, content, StandardCharsets.UTF_8);
        try {
          Files.move(temp, file, StandardCopyOption.REPLACE_EXISTING,
              StandardCopyOption.ATOMIC_MOVE);
        } catch (AtomicMoveNotSupportedException e) {
          Files.move(temp, file, StandardCopyOption.REPLACE_EXISTING);
        }
      } finally {
        Files.deleteIfExists(temp);
      }
    } catch (IOException e) {
      throw new CraftforjActionException("Failed to write stylesheet: " + file, e);
    }
  }

  private static Set<String> topLevelPreludes(String css) {
    Set<String> preludes = new LinkedHashSet<>();
    StringBuilder prelude = new StringBuilder();
    int depth = 0;
    int i = 0;

    while (i < css.length()) {
      char c = css.charAt(i);

      if (c == '/' && i + 1 < css.length() && css.charAt(i + 1) == '*') {
        int end = css.indexOf("*/", i + 2);
        i = end < 0 ? css.length() : end + 2;
      } else if (c == '"' || c == '\'') {
        int end = skipString(css, i);
        if (depth == 0) {
          prelude.append(css, i, end);
        }
        i = end;
      } else if (c == '{') {
        if (depth == 0) {
          String name = normalize(prelude.toString());
          if (!name.isEmpty()) {
            preludes.add(name);
          }
          prelude.setLength(0);
        }
        depth++;
        i++;
      } else if (c == '}') {
        depth = Math.max(0, depth - 1);
        i++;
      } else if (c == ';' && depth == 0) {
        prelude.setLength(0);
        i++;
      } else {
        if (depth == 0) {
          prelude.append(c);
        }
        i++;
      }
    }

    return preludes;
  }

  private static int skipString(String css, int start) {
    char quote = css.charAt(start);
    int i = start + 1;

    while (i < css.length()) {
      char c = css.charAt(i);
      if (c == '\\') {
        i += 2;
        continue;
      }

      if (c == quote) {
        return i + 1;
      }

      i++;
    }

    return css.length();
  }

  private static String normalize(String prelude) {
    return prelude.trim().replaceAll("\\s+", " ");
  }

  private static int countOccurrences(String content, String text) {
    int count = 0;
    int index = content.indexOf(text);

    while (index >= 0) {
      count++;
      index = content.indexOf(text, index + text.length());
    }

    return count;
  }

  private static String snippet(String text) {
    String flat = text.replace("\n", "\\n");
    if (flat.length() <= SNIPPET_LENGTH) {
      return flat;
    }

    return flat.substring(0, SNIPPET_LENGTH) + "...";
  }
}
