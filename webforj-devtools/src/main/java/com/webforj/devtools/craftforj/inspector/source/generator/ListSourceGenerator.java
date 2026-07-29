package com.webforj.devtools.craftforj.inspector.source.generator;

import com.github.javaparser.ast.expr.Expression;
import com.webforj.devtools.craftforj.inspector.source.SourceModificationException;
import java.util.ArrayList;
import java.util.List;

/**
 * Source generator for list values.
 *
 * <p>
 * Generates varargs calls like {@code button.addClassName("class1", "class2")}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class ListSourceGenerator implements SourceGenerator {

  /**
   * {@inheritDoc}
   */
  @Override
  public SourceChange generate(GeneratorContext context) {
    Object value = context.getValue();
    if (!(value instanceof List<?>)) {
      String actualType = value == null ? "null" : value.getClass().getSimpleName();
      throw new SourceModificationException(
          "Property '" + context.getMethodName() + "' expects a list but received: " + actualType);
    }

    List<?> items = (List<?>) value;
    if (items.isEmpty()) {
      return null;
    }

    try {
      List<Expression> args = new ArrayList<>();
      for (Object item : items) {
        args.add(ScalarSourceGenerator.toExpression(item));
      }
      return SourceChange.builder().methodCall(context.getMethodName(), args).build();
    } catch (SourceModificationException e) {
      throw new SourceModificationException(
          "Property '" + context.getMethodName() + "': " + e.getMessage());
    }
  }
}
