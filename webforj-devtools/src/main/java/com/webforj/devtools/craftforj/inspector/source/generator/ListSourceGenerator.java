package com.webforj.devtools.craftforj.inspector.source.generator;

import com.github.javaparser.ast.expr.Expression;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.model.SourceChange;
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

  private final Class<?> itemType;
  private final boolean replaceAllCalls;

  /** Creates a generator for untyped, replacing varargs methods. */
  public ListSourceGenerator() {
    this(null, false);
  }

  /**
   * Creates a generator with the list contribution's element and accumulation contract.
   *
   * @param itemType the required element type, or null for untyped values
   * @param replaceAllCalls whether the values replace all accumulating calls
   */
  public ListSourceGenerator(Class<?> itemType, boolean replaceAllCalls) {
    this.itemType = itemType;
    this.replaceAllCalls = replaceAllCalls;
  }

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
      for (int index = 0; index < items.size(); index++) {
        Object item = items.get(index);
        if (itemType != null && !itemType.isInstance(item)) {
          throw new SourceModificationException("expects " + itemType.getSimpleName()
              + " items but item " + (index + 1) + " is not a " + itemType.getSimpleName());
        }
        args.add(ScalarSourceGenerator.toExpression(item));
      }
      return SourceChange.builder().methodCall(context.getMethodName(), args)
          .replaceAllCalls(replaceAllCalls).build();
    } catch (SourceModificationException e) {
      throw new SourceModificationException(
          "Property '" + context.getMethodName() + "': " + e.getMessage());
    }
  }
}
