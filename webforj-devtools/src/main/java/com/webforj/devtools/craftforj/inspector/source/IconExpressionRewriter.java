package com.webforj.devtools.craftforj.inspector.source;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.webforj.devtools.craftforj.inspector.source.generator.IconExpressionGenerator;
import com.webforj.devtools.craftforj.inspector.source.generator.IconExpressionGenerator.IconExpression;
import com.webforj.devtools.craftforj.inspector.source.generator.IconExpressionGenerator.IconValue;
import com.webforj.devtools.craftforj.inspector.source.model.TargetContext;
import com.webforj.devtools.craftforj.inspector.source.parser.AstFinder;
import java.util.List;

/**
 * Rewrites the icon expression of an {@code Icon} component in place.
 *
 * <p>
 * Icon changes replace the originating expression instead of appending setter calls: a factory call
 * ({@code TablerIcon.create("inbox")}, {@code FeatherIcon.BELL.create()}) is swapped for the
 * canonical factory expression of the new pool, while an {@code Icon}/{@code IconButton} creation
 * ({@code new Icon("home", "tabler")}) keeps its constructor and gets its name and pool literals
 * updated, preserving any chained calls and the surrounding code.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class IconExpressionRewriter {

  private IconExpressionRewriter() {}

  /**
   * Rewrites the icon expression at the target line to the given {@code "pool:name"} value.
   *
   * @param cu the compilation unit to modify
   * @param target the target context identifying the icon component
   * @param value the new icon value in {@code "pool:name"} format
   *
   * @return the fully qualified names the rewritten expression requires as imports
   * @throws SourceModificationException if no unambiguous icon expression is found or the
   *         expression cannot be rewritten
   */
  public static List<String> rewrite(CompilationUnit cu, TargetContext target, Object value) {
    List<Expression> candidates = AstFinder.findIconExpressionsAt(cu, target);

    if (candidates.isEmpty()) {
      throw new SourceModificationException(
          "No icon expression found at line " + target.getLineNumber());
    }

    if (candidates.size() > 1) {
      throw new SourceModificationException("Multiple icon expressions at line "
          + target.getLineNumber() + ". Cannot determine which one to modify.");
    }

    Expression expression = candidates.get(0);
    if (expression instanceof ObjectCreationExpr creation) {
      return rewriteCreation(creation, value);
    }

    IconExpression generated = IconExpressionGenerator.generate(String.valueOf(value));
    expression.replace(generated.expression());

    return generated.imports();
  }

  private static List<String> rewriteCreation(ObjectCreationExpr creation, Object value) {
    boolean hasLiteralArgs =
        creation.getArguments().size() >= 2 && creation.getArgument(0) instanceof StringLiteralExpr
            && creation.getArgument(1) instanceof StringLiteralExpr;

    if (!hasLiteralArgs) {
      throw new SourceModificationException(
          "Cannot rewrite icon creation 'new " + creation.getType().getNameAsString()
              + "(...)'. Expected string literal name and pool arguments.");
    }

    IconValue parsed = IconExpressionGenerator.parseValue(value);
    creation.getArgument(0).replace(new StringLiteralExpr(parsed.name()));
    creation.getArgument(1).replace(new StringLiteralExpr(parsed.pool()));

    return List.of();
  }
}
