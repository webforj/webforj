package com.webforj.devtools.craftforj.inspector.source.parser;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Line-based import editor for printed source.
 *
 * <p>
 * Import edits through the AST make {@code LexicalPreservingPrinter} eat blank lines around the
 * import block, so imports are synced textually on the printed output instead.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class ImportWriter {

  private ImportWriter() {}

  /**
   * Adds an import for every used candidate and removes the imports of unused ones.
   *
   * @param source the printed source code
   * @param candidates the fully qualified names this edit manages
   * @param used the subset of candidates referenced by the source
   * @return the source with the candidate imports synced
   */
  public static String sync(String source, Collection<String> candidates, Set<String> used) {
    List<String> lines = new ArrayList<>(Arrays.asList(source.split("\n", -1)));

    Set<String> unusedImports = candidates.stream().filter(name -> !used.contains(name))
        .map(name -> "import " + name + ";").collect(Collectors.toSet());
    lines.removeIf(line -> unusedImports.contains(line.trim()));

    for (String name : used) {
      String importLine = "import " + name + ";";
      if (lines.stream().noneMatch(line -> line.trim().equals(importLine))) {
        insert(lines, importLine);
      }
    }

    return String.join("\n", lines);
  }

  private static void insert(List<String> lines, String importLine) {
    int lastImport = -1;
    int packageLine = -1;
    int sortedPosition = -1;
    for (int i = 0; i < lines.size(); i++) {
      String trimmed = lines.get(i).trim();
      if (trimmed.startsWith("import ")) {
        lastImport = i;
        if (sortedPosition < 0 && trimmed.compareTo(importLine) > 0) {
          sortedPosition = i;
        }
      } else if (trimmed.startsWith("package ")) {
        packageLine = i;
      }
    }

    if (sortedPosition >= 0) {
      lines.add(sortedPosition, importLine);
    } else if (lastImport >= 0) {
      lines.add(lastImport + 1, importLine);
    } else if (packageLine >= 0) {
      lines.add(packageLine + 1, importLine);
      lines.add(packageLine + 1, "");
    } else {
      lines.add(0, importLine);
    }
  }
}
