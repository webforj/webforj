package com.webforj.devtools.craftforj.inspector.contribution.layout.columnslayout.item;

import com.webforj.component.Component;
import com.webforj.component.layout.columnslayout.ColumnsLayout;
import com.webforj.concern.HasStyle;
import com.webforj.devtools.craftforj.inspector.contribution.LayoutItemContribution;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;

/**
 * Base contribution for columns layout item properties.
 *
 * <p>
 * Columns layout item properties are applied through the parent {@link ColumnsLayout}'s item API,
 * e.g. {@code columnsLayout.setSpan(item, 2)}, both for live changes and generated source code. The
 * item is the first argument in this API, unlike FlexLayout's item-last convention.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public abstract class ColumnsLayoutItemContribution extends LayoutItemContribution<ColumnsLayout> {

  /**
   * Creates a columns layout item contribution.
   *
   * @param concernInterface the interface the child must implement (HasAttribute or HasStyle)
   * @param displayName the display name shown in UI (e.g., "Span")
   * @param methodName the ColumnsLayout item API method (e.g., "setSpan")
   */
  protected ColumnsLayoutItemContribution(Class<?> concernInterface, String displayName,
      String methodName) {
    super(ColumnsLayout.class, concernInterface, displayName, methodName,
        SourceChange.ItemPosition.FIRST);
  }

  /**
   * Parses an attribute value as an integer.
   *
   * @param value the raw attribute value
   * @return the parsed integer, or null when absent or not a number
   */
  protected static Integer parseInt(String value) {
    if (value == null || value.isEmpty()) {
      return null;
    }
    try {
      return Integer.parseInt(value);
    } catch (NumberFormatException e) {
      return null;
    }
  }

  /**
   * Converts a client value to an int.
   *
   * @param value the client value (Number or numeric string)
   * @return the int value
   */
  protected static int toInt(Object value) {
    return (value instanceof Number n) ? n.intValue() : Integer.parseInt(String.valueOf(value));
  }

  /**
   * Resolves a ColumnsLayout alignment from a fully qualified enum value.
   *
   * @param value the fully qualified enum constant (e.g., "...ColumnsLayout.Alignment.CENTER")
   * @return the alignment
   */
  protected static ColumnsLayout.Alignment parseAlignment(Object value) {
    String name = String.valueOf(value);
    return ColumnsLayout.Alignment.valueOf(name.substring(name.lastIndexOf('.') + 1));
  }

  /**
   * Reads a style-backed alignment from the child and maps it to its fully qualified enum value.
   *
   * @param component the child component
   * @param styleKey the style key the parent writes ("justify-self" or "align-self")
   * @return the fully qualified enum constant, or null when unset
   */
  protected static String readAlignment(Component component, String styleKey) {
    String value = ((HasStyle<?>) component).getStyle(styleKey);
    if (value == null || value.isEmpty()) {
      return null;
    }

    for (ColumnsLayout.Alignment alignment : ColumnsLayout.Alignment.values()) {
      if (alignment.getValue().equals(value)) {
        return ColumnsLayout.Alignment.class.getCanonicalName() + "." + alignment.name();
      }
    }

    return null;
  }
}
