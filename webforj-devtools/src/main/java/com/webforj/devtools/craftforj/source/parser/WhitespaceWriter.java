package com.webforj.devtools.craftforj.source.parser;

import com.github.javaparser.Range;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.BodyDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.TypeDeclaration;
import com.github.javaparser.ast.comments.LineComment;
import com.github.javaparser.ast.expr.TextBlockLiteralExpr;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.github.javaparser.ast.stmt.SwitchEntry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Repairs the whitespace around the members and statements a source modification added.
 *
 * <p>
 * The lexical printer loses the indentation of a statement added at the top of a block, indents the
 * inside of a new block by a unit of its own, opens a block that replaces a body on a line of its
 * own and leaves stray whitespace around a new member. This writer runs on the printed text.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class WhitespaceWriter {

  private static final Pattern CLOSING = Pattern.compile("\\)+[;,]?");
  private static final Pattern HEADER = Pattern.compile(".*(\\)|\\belse|\\bdo)");

  private final List<String> before;
  private final Set<String> written;
  private final List<String> lines;
  private final String unit;
  private final List<Node> added = new ArrayList<>();
  private final Set<Integer> dropped = new HashSet<>();
  private final Set<Integer> spaced = new HashSet<>();
  private final Set<Integer> starts = new HashSet<>();
  private final Set<Integer> commented = new HashSet<>();
  private int appended = -1;

  private WhitespaceWriter(String original, String modified) {
    this.before = original.lines().toList();
    this.written = new HashSet<>(before);
    this.lines = new ArrayList<>(Arrays.asList(modified.split("\n", -1)));
    this.unit = StatementWrapper.detectIndentUnit(original);
  }

  /**
   * Repairs the whitespace of what this modification added.
   *
   * @param original the file content before the modification
   * @param modified the printed content after the modification
   *
   * @return the modified content with the whitespace of the added code repaired
   */
  public static String repair(String original, String modified) {
    if (original.contains("\r\n")) {
      String repaired = repair(original.replace("\r\n", "\n"), modified.replace("\r\n", "\n"));

      return repaired.replace("\n", "\r\n");
    }

    CompilationUnit cu = SourceParserService.getCurrent().parse(modified).orElse(null);

    return cu == null ? modified : new WhitespaceWriter(original, modified).write(cu);
  }

  private String write(CompilationUnit cu) {
    cu.getAllComments().stream().filter(LineComment.class::isInstance).forEach(
        comment -> comment.getRange().ifPresent(range -> commented.add(range.end.line - 1)));
    cu.walk(Node.TreeTraversal.PREORDER, this::collect);
    added.forEach(this::indent);
    added.forEach(this::space);
    cu.walk(Node.TreeTraversal.PREORDER, this::repairNode);

    return join();
  }

  private void collect(Node node) {
    if (!startsOwnLine(node)) {
      return;
    }

    node.getRange().ifPresent(range -> starts.add(range.begin.line - 1));
    if (isAdded(node)) {
      added.add(node);
    }
  }

  private void repairNode(Node node) {
    if (node instanceof BlockStmt block) {
      trimBlock(block);
      closeEmptyBlock(block);
      joinOpening(block);
    } else if (startsOwnLine(node) && !added.contains(node)) {
      if (node instanceof Statement || node instanceof FieldDeclaration) {
        restoreIndent(node);
      }

      if (node instanceof BodyDeclaration<?> member) {
        restoreBlankLine(member);
      }
    }
  }

  private String join() {
    List<String> out = new ArrayList<>();
    for (int i = 0; i < lines.size(); i++) {
      if (!isLeftOut(i)) {
        append(out, i);
      }
    }

    return String.join("\n", out);
  }

  private void append(List<String> out, int index) {
    String line = lines.get(index);
    // What is joined to a line that ends in a comment becomes a part of the comment
    boolean joinable = !out.isEmpty() && !commented.contains(appended);
    appended = index;
    if (isDanglingClosing(line) && joinable) {
      out.set(out.size() - 1, out.get(out.size() - 1).stripTrailing() + line.strip());

      return;
    }

    if (spaced.contains(index) && !line.isBlank()) {
      out.add("");
    }

    out.add(line.isBlank() ? "" : line);
  }

  // A whitespace line the printer produced goes, unless it is the blank line a member needs
  private boolean isLeftOut(int index) {
    String line = lines.get(index);
    boolean stray = !line.isEmpty() && line.isBlank() && !written.contains(line);

    return dropped.contains(index) || (stray && !spaced.contains(index));
  }

  // The printer leaves the closing of a call alone on a line when it removes the last argument
  private boolean isDanglingClosing(String line) {
    return !written.contains(line) && CLOSING.matcher(line.strip()).matches();
  }

  private boolean isAdded(Node node) {
    Range range = node.getRange().orElse(null);
    if (range == null) {
      return false;
    }

    String line = lines.get(range.begin.line - 1);

    return !written.contains(line)
        && line.substring(0, Math.min(range.begin.column - 1, line.length())).isBlank();
  }

  private void indent(Node node) {
    Range range = node.getRange().orElseThrow();
    Node block = findOpener(node.getParentNode().orElseThrow());
    int opening = block instanceof TypeDeclaration<?> type
        ? type.getName().getRange().orElseThrow().begin.line
        : block.getRange().orElseThrow().begin.line;

    String expected = getIndent(lines.get(opening - 1)) + unit;
    String base = getWrittenIndent(node, expected);
    String first = lines.get(range.begin.line - 1);
    lines.set(range.begin.line - 1, expected + first.stripLeading());
    if (expected.equals(base)) {
      return;
    }

    for (int i = range.begin.line; i < range.end.line; i++) {
      String line = lines.get(i);
      String lead = getIndent(line);
      if (!line.isBlank()) {
        String rest = lead.startsWith(base) ? lead.substring(base.length()) : "";
        lines.set(i, expected + rest + line.substring(lead.length()));
      }
    }
  }

  // The brace of a block stands on the last line of what opens the block, which is a wrapped
  // line when the signature or the condition takes more than one. The first line tells the
  // indentation.
  private static Node findOpener(Node block) {
    Node holder = block.getParentNode().orElse(null);
    boolean listed = holder instanceof BlockStmt || holder instanceof SwitchEntry;

    return block instanceof BlockStmt && holder != null && !listed ? holder : block;
  }

  // Code moved as it was written keeps the indentation of the place it came from. The line that
  // closes it tells that place, wrapped arguments tell it only when no text block could be hurt
  private String getWrittenIndent(Node node, String expected) {
    Range range = node.getRange().orElseThrow();
    String actual = getIndent(lines.get(range.begin.line - 1));
    String last = lines.get(range.end.line - 1).stripLeading();
    if (range.end.line == range.begin.line) {
      return actual;
    }

    if (last.startsWith("}") || last.startsWith(")")) {
      return getIndent(lines.get(range.end.line - 1));
    }

    if (!expected.equals(actual) || node.findFirst(TextBlockLiteralExpr.class).isPresent()) {
      return actual;
    }

    String continuation = unit + unit;
    String shortest = getShortestIndent(range.begin.line, range.end.line);

    return shortest != null && shortest.endsWith(continuation)
        ? shortest.substring(0, shortest.length() - continuation.length())
        : actual;
  }

  private String getShortestIndent(int from, int to) {
    String shortest = null;
    for (int i = from; i < to; i++) {
      String lead = getIndent(lines.get(i));
      if (!lines.get(i).isBlank() && (shortest == null || lead.length() < shortest.length())) {
        shortest = lead;
      }
    }

    return shortest;
  }

  // Fields stand together, anything else is set apart by one blank line
  private void space(Node node) {
    if (!(node instanceof BodyDeclaration<?> member)) {
      return;
    }

    TypeDeclaration<?> type = (TypeDeclaration<?>) member.getParentNode().orElseThrow();
    int index = type.getMembers().indexOf(member);
    if (index > 0) {
      separate(type.getMember(index - 1), member, true);
    } else {
      int opening = type.getName().getRange().orElseThrow().begin.line;
      dropped.addAll(findBlanks(opening, getFirstLine(member) - 1));
    }

    if (index < type.getMembers().size() - 1) {
      separate(member, type.getMember(index + 1), false);
    }
  }

  private void separate(BodyDeclaration<?> first, BodyDeclaration<?> second, boolean joining) {
    int to = getFirstLine(second) - 1;
    List<Integer> blanks = findBlanks(first.getRange().orElseThrow().end.line, to);
    boolean together = first instanceof FieldDeclaration && second instanceof FieldDeclaration;

    if (together && joining) {
      dropped.addAll(blanks);
    } else if (!together && blanks.isEmpty()) {
      spaced.add(to);
    } else if (!together) {
      spaced.add(blanks.get(0));
      dropped.addAll(blanks.subList(1, blanks.size()));
    }
  }

  private List<Integer> findBlanks(int from, int to) {
    List<Integer> blanks = new ArrayList<>();
    for (int i = from; i < to; i++) {
      if (lines.get(i).isBlank()) {
        blanks.add(i);
      }
    }

    return blanks;
  }

  // The printer eats the blank line above a member when it removes the member before it
  private void restoreBlankLine(BodyDeclaration<?> member) {
    int first = getFirstLine(member) - 1;
    TypeDeclaration<?> type = (TypeDeclaration<?>) member.getParentNode().orElseThrow();
    if (first == 0 || lines.get(first - 1).isBlank() || type.getMembers().indexOf(member) == 0) {
      return;
    }

    int at = before.indexOf(lines.get(first));
    if (at > 0 && at == before.lastIndexOf(lines.get(first)) && before.get(at - 1).isBlank()) {
      spaced.add(first);
    }
  }

  // The printer eats a part of the indentation of the argument that follows one it removed, the
  // lines below the first line of the statement tell what the developer wrote
  private void restoreIndent(Node node) {
    Range range = node.getRange().orElse(null);
    boolean moved = added.stream().anyMatch(other -> other.isAncestorOf(node));
    if (range == null || range.begin.line == range.end.line || moved) {
      return;
    }

    String first = lines.get(range.begin.line - 1);
    int at = findWrittenLine(range.begin.line - 1);
    if (at < 0) {
      return;
    }

    for (int i = range.begin.line; i < range.end.line; i++) {
      String line = lines.get(i);
      if (!line.isBlank() && !written.contains(line) && !starts.contains(i)) {
        lines.set(i, findWritten(at, getIndent(first), line.strip()).orElse(line));
      }
    }
  }

  // A line written several times is told apart by its place among its copies, which holds as
  // long as the modification left their number alone
  private int findWrittenLine(int index) {
    String line = lines.get(index);
    List<Integer> copies = findCopies(before, line);
    List<Integer> printed = findCopies(lines, line);

    return copies.size() == printed.size() ? copies.get(printed.indexOf(index)) : -1;
  }

  private static List<Integer> findCopies(List<String> content, String line) {
    List<Integer> copies = new ArrayList<>();
    for (int i = 0; i < content.size(); i++) {
      if (content.get(i).equals(line)) {
        copies.add(i);
      }
    }

    return copies;
  }

  private Optional<String> findWritten(int from, String indent, String text) {
    for (int i = from + 1; i < before.size(); i++) {
      String line = before.get(i);
      if (!line.isBlank() && getIndent(line).length() <= indent.length()) {
        break;
      }

      if (line.strip().equals(text)) {
        return Optional.of(line);
      }
    }

    return Optional.empty();
  }

  // The printer leaves a blank line at the edge of a block when it removes the statement there
  private void trimBlock(BlockStmt block) {
    Range range = block.getRange().orElse(null);
    if (range == null || range.end.line - range.begin.line < 2) {
      return;
    }

    for (int edge : new int[] {range.begin.line, range.end.line - 2}) {
      if (lines.get(edge).isBlank() && !isWritten(edge - 1, edge + 1)) {
        dropped.add(edge);
      }
    }
  }

  // The printer drops the indentation of the brace that closes a block it emptied
  private void closeEmptyBlock(BlockStmt block) {
    Range range = block.getRange().orElse(null);
    if (range == null || range.begin.line == range.end.line || !block.getStatements().isEmpty()) {
      return;
    }

    int opening = range.begin.line - 1;
    int closing = range.end.line - 1;
    String expected = getIndent(lines.get(opening)) + lines.get(closing).stripLeading();
    boolean alone = lines.get(closing).strip().equals("}");
    if (alone && !isWritten(opening, closing)) {
      lines.set(closing, expected);
    }
  }

  // The printer opens the block that takes the place of a body without braces on the line the
  // body stood on
  private void joinOpening(BlockStmt block) {
    Range range = block.getRange().orElse(null);
    Node holder = block.getParentNode().orElse(null);
    boolean body = holder instanceof Statement && !(holder instanceof BlockStmt);
    if (range == null || !body || range.begin.line < 2) {
      return;
    }

    int opening = range.begin.line - 1;
    String header = lines.get(opening - 1);
    boolean printed =
        lines.get(opening).strip().equals("{") && !written.contains(lines.get(opening));
    if (!printed || !HEADER.matcher(header.strip()).matches()) {
      return;
    }

    lines.set(opening - 1, header.stripTrailing() + " {");
    dropped.add(opening);
    int closing = range.end.line - 1;
    if (closing > opening && lines.get(closing).strip().equals("}")) {
      lines.set(closing, getIndent(header) + "}");
    }
  }

  // Two lines count as written by the developer when the original holds them the same distance
  // apart with only blank lines between
  private boolean isWritten(int first, int second) {
    int gap = second - first;
    for (int i = 0; i + gap < before.size(); i++) {
      boolean same =
          before.get(i).equals(lines.get(first)) && before.get(i + gap).equals(lines.get(second));
      if (same && before.subList(i + 1, i + gap).stream().allMatch(String::isBlank)) {
        return true;
      }
    }

    return false;
  }

  private static boolean startsOwnLine(Node node) {
    Node parent = node.getParentNode().orElse(null);

    return (node instanceof Statement && parent instanceof BlockStmt)
        || (node instanceof BodyDeclaration && parent instanceof TypeDeclaration);
  }

  private static int getFirstLine(BodyDeclaration<?> member) {
    int line = member.getRange().orElseThrow().begin.line;

    return member.getComment().flatMap(Node::getRange)
        .map(range -> Math.min(line, range.begin.line)).orElse(line);
  }

  private static String getIndent(String line) {
    return line.substring(0, line.length() - line.stripLeading().length());
  }
}
