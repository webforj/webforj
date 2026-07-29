package com.webforj.devtools.craftforj.inspector.source.generator;

import com.github.javaparser.ast.expr.Expression;
import com.webforj.devtools.craftforj.inspector.source.SourceModificationException;
import java.util.List;
import java.util.Map;

/**
 * Source generator for key-value methods like setStyle(key, value).
 *
 * <p>
 * Expects value to be either:
 * </p>
 * <ul>
 * <li>A List with two elements: [key, value]</li>
 * <li>A Map with "key" and "value" entries</li>
 * </ul>
 *
 * <p>
 * Generates two-argument method calls: {@code component.setStyle("flex-grow", "1")}
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class KeyValueSourceGenerator implements SourceGenerator {

  /**
   * {@inheritDoc}
   */
  @Override
  public SourceChange generate(GeneratorContext context) {
    Object value = context.getValue();

    if (value == null) {
      return null;
    }

    Object key;
    Object val;

    if (value instanceof List<?> list) {
      if (list.size() < 2) {
        return null;
      }
      key = list.get(0);
      val = list.get(1);
    } else if (value instanceof Map<?, ?> map) {
      key = map.get("key");
      val = map.get("value");
    } else {
      throw new SourceModificationException(
          "KeyValue property expects List or Map, got: " + value.getClass().getSimpleName());
    }

    // Empty value means remove
    if (val == null || (val instanceof String s && s.isEmpty())) {
      return null;
    }

    if (key == null) {
      throw new SourceModificationException("KeyValue property missing key");
    }

    Expression keyExpr = ScalarSourceGenerator.toExpression(key);
    Expression valExpr = ScalarSourceGenerator.toExpression(val);

    return SourceChange.builder().methodCall(context.getMethodName(), List.of(keyExpr, valExpr))
        .matchKey(String.valueOf(key)).build();
  }
}
