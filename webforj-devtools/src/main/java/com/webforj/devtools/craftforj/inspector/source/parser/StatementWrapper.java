package com.webforj.devtools.craftforj.inspector.source.parser;

import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.stmt.ExpressionStmt;
import com.github.javaparser.ast.stmt.Statement;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Wraps over-long statement lines produced by a source modification.
 *
 * <p>
 * The lexical printer emits inserted and rewritten statements on a single line regardless of
 * length. This wrapper runs on the printed text right before it is written back: any line that
 * exceeds the length limit and does not exist verbatim in the original file (so it was produced by
 * this write, never by the user) is re-parsed and broken at argument boundaries using the file's
 * own indentation unit. Untouched lines pass through byte-identical.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class StatementWrapper {

  static final int MAX_LINE_LENGTH = 100;

  private StatementWrapper() {}

  /**
   * Wraps the over-long lines introduced by this modification.
   *
   * @param original the file content before the modification
   * @param modified the printed content after the modification
   * @return the modified content with newly introduced over-long statements wrapped
   */
  public static String wrap(String original, String modified) {
    Set<String> originalLines = new HashSet<>(original.lines().toList());
    String unit = detectIndentUnit(original);
    String[] lines = modified.split("\n", -1);
    StringBuilder out = new StringBuilder(modified.length());

    for (int i = 0; i < lines.length; i++) {
      String line = lines[i];
      String replacement = line;
      if (line.length() > MAX_LINE_LENGTH && !originalLines.contains(line)) {
        String wrapped = wrapLine(line, unit);
        if (wrapped != null) {
          replacement = wrapped;
        }
      }

      out.append(replacement);
      if (i < lines.length - 1) {
        out.append('\n');
      }
    }

    return out.toString();
  }

  private static String wrapLine(String line, String unit) {
    String code = line.stripLeading();
    String indent = line.substring(0, line.length() - code.length());
    if (!code.endsWith(";")) {
      return null;
    }

    Statement statement = parseStatement(code);
    if (!(statement instanceof ExpressionStmt expressionStmt)
        || !(expressionStmt.getExpression() instanceof MethodCallExpr call)) {
      return null;
    }

    List<String> parts = new ArrayList<>();
    emitCall(parts, call, indent, indent + unit + unit, unit, ";");

    return parts.size() > 1 ? String.join("\n", parts) : null;
  }

  private static void emitCall(List<String> out, MethodCallExpr call, String indent,
      String continuation, String unit, String suffix) {
    String flat = indent + call + suffix;
    if (flat.length() <= MAX_LINE_LENGTH || call.getArguments().isEmpty()) {
      out.add(flat);
      return;
    }

    String head = indent + callPrefix(call);
    NodeList<Expression> arguments = call.getArguments();
    if (arguments.size() == 1 && arguments.get(0) instanceof MethodCallExpr inner
        && !inner.getArguments().isEmpty()) {
      emitArguments(out, head + callPrefix(inner), inner.getArguments(), continuation, unit,
          "))" + suffix);

      return;
    }

    emitArguments(out, head, arguments, continuation, unit, ")" + suffix);
  }

  private static void emitArguments(List<String> out, String head, NodeList<Expression> arguments,
      String continuation, String unit, String closing) {
    out.add(head);

    for (int i = 0; i < arguments.size(); i++) {
      Expression argument = arguments.get(i);
      String tail = i == arguments.size() - 1 ? closing : ",";
      String line = continuation + argument + tail;
      if (line.length() > MAX_LINE_LENGTH && argument instanceof MethodCallExpr nested) {
        emitCall(out, nested, continuation, continuation + unit + unit, unit, tail);
      } else {
        out.add(line);
      }
    }
  }

  private static String callPrefix(MethodCallExpr call) {
    return call.getScope().map(scope -> scope + ".").orElse("") + call.getNameAsString() + "(";
  }

  private static String detectIndentUnit(String content) {
    for (String line : (Iterable<String>) content.lines()::iterator) {
      String stripped = line.stripLeading();
      // Javadoc and block comment continuations indent one extra space; they never reflect the
      // file's indentation unit.
      if (stripped.isEmpty() || stripped.startsWith("*")) {
        continue;
      }

      int width = line.length() - stripped.length();
      if (width > 0) {
        return line.substring(0, width);
      }
    }

    return "  ";
  }

  private static Statement parseStatement(String code) {
    return SourceParserService.getCurrent().parseStatement(code);
  }
}
