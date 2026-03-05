package com.webforj.component.table.renderer;

/**
 * Represents a client-side condition for use with {@link ConditionalRenderer}.
 *
 * <p>
 * Conditions produce JavaScript boolean expressions evaluated against cell data and can be composed
 * with {@link #and(Condition)} and {@link #or(Condition)}.
 * </p>
 *
 * <pre>{@code
 * Condition inRange = Condition.greaterThan(50).and(Condition.lessThan(100));
 * Condition isActive = Condition.equalTo("Active");
 * Condition combined = isActive.or(inRange);
 * }</pre>
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
public class Condition {
  private static final String CELL_VALUE = "cell.value";
  private final String expression;

  private Condition(String expression) {
    this.expression = expression;
  }

  /**
   * Returns the JavaScript boolean expression.
   *
   * @return the expression string
   */
  public String toExpression() {
    return expression;
  }

  /**
   * Creates a condition that matches when the cell value equals the given string.
   *
   * @param value the value to match
   * @return the condition
   */
  public static Condition equalTo(String value) {
    return buildEqualTo(CELL_VALUE, value);
  }

  /**
   * Creates a condition that matches when the cell value equals the given number.
   *
   * @param value the value to match
   * @return the condition
   */
  public static Condition equalTo(Number value) {
    return buildEqualToNumber(CELL_VALUE, value);
  }

  /**
   * Creates a condition that matches when the cell value equals the given string, ignoring case.
   *
   * @param value the value to match
   * @return the condition
   */
  public static Condition equalToIgnoreCase(String value) {
    return buildEqualToIgnoreCase(CELL_VALUE, value);
  }

  /**
   * Creates a condition that matches when the cell value is one of the given strings.
   *
   * @param values the values to match
   * @return the condition
   */
  public static Condition in(String... values) {
    return buildIn(CELL_VALUE, false, values);
  }

  /**
   * Creates a condition that matches when the cell value is one of the given strings, ignoring
   * case.
   *
   * @param values the values to match
   * @return the condition
   */
  public static Condition inIgnoreCase(String... values) {
    return buildIn(CELL_VALUE, true, values);
  }

  /**
   * Creates a condition that matches when the cell value is greater than the given number.
   *
   * @param value the threshold
   * @return the condition
   */
  public static Condition greaterThan(Number value) {
    return buildCompare(CELL_VALUE, ">", value);
  }

  /**
   * Creates a condition that matches when the cell value is greater than or equal to the given
   * number.
   *
   * @param value the threshold
   * @return the condition
   */
  public static Condition greaterThanOrEqual(Number value) {
    return buildCompare(CELL_VALUE, ">=", value);
  }

  /**
   * Creates a condition that matches when the cell value is less than the given number.
   *
   * @param value the threshold
   * @return the condition
   */
  public static Condition lessThan(Number value) {
    return buildCompare(CELL_VALUE, "<", value);
  }

  /**
   * Creates a condition that matches when the cell value is less than or equal to the given number.
   *
   * @param value the threshold
   * @return the condition
   */
  public static Condition lessThanOrEqual(Number value) {
    return buildCompare(CELL_VALUE, "<=", value);
  }

  /**
   * Creates a condition that matches when the cell value is between the given min and max
   * (inclusive).
   *
   * @param min the minimum value
   * @param max the maximum value
   * @return the condition
   */
  public static Condition between(Number min, Number max) {
    return buildBetween(CELL_VALUE, min, max);
  }

  /**
   * Creates a condition that matches when the cell value is null, undefined, or empty.
   *
   * @return the condition
   */
  public static Condition isEmpty() {
    return buildIsNull(CELL_VALUE);
  }

  /**
   * Creates a condition that matches when the cell value is true.
   *
   * @return the condition
   */
  public static Condition isTrue() {
    return buildIsTrue(CELL_VALUE);
  }

  /**
   * Creates a condition that matches when the cell value is false.
   *
   * @return the condition
   */
  public static Condition isFalse() {
    return buildIsFalse(CELL_VALUE);
  }

  /**
   * Creates a condition that matches when the cell value contains the given text.
   *
   * @param text the text to search for
   * @return the condition
   */
  public static Condition contains(String text) {
    return buildContains(CELL_VALUE, text);
  }

  /**
   * Creates a condition that matches when the cell value contains the given text, ignoring case.
   *
   * @param text the text to search for
   * @return the condition
   */
  public static Condition containsIgnoreCase(String text) {
    return buildContainsIgnoreCase(CELL_VALUE, text);
  }

  /**
   * Creates a condition from a raw JavaScript expression.
   *
   * <p>
   * The expression has access to {@code cell}, {@code cell.value}, {@code cell.row}, and
   * {@code cell.column}.
   * </p>
   *
   * @param js the JavaScript boolean expression
   * @return the condition
   */
  public static Condition expression(String js) {
    return new Condition(js);
  }

  /**
   * Returns a column-scoped condition builder that targets the given column's value instead of the
   * current cell's value.
   *
   * @param columnKey the column key to target
   * @return a condition builder scoped to the given column
   */
  public static ColumnConditionBuilder column(String columnKey) {
    return new ColumnConditionBuilder(columnKey);
  }

  /**
   * Returns a new condition that is the logical AND of this condition and the given condition.
   *
   * @param other the other condition
   * @return the combined condition
   */
  public Condition and(Condition other) {
    return new Condition("(" + this.expression + ") && (" + other.expression + ")");
  }

  /**
   * Returns a new condition that is the logical OR of this condition and the given condition.
   *
   * @param other the other condition
   * @return the combined condition
   */
  public Condition or(Condition other) {
    return new Condition("(" + this.expression + ") || (" + other.expression + ")");
  }

  /**
   * Returns a new condition that is the logical negation of this condition.
   *
   * @return the negated condition
   */
  public Condition negate() {
    return new Condition("!(" + this.expression + ")");
  }

  /**
   * A condition builder that targets a specific column's value instead of the current cell's value.
   *
   * @see Condition#column(String)
   * @since 25.12
   */
  public static class ColumnConditionBuilder {
    private final String valueExpr;

    private ColumnConditionBuilder(String columnKey) {
      this.valueExpr = "cell.row.getValue('" + escape(columnKey) + "')";
    }

    /**
     * Creates a condition that matches when the column value equals the given string.
     *
     * @param value the value to match
     * @return the condition
     */
    public Condition equalTo(String value) {
      return buildEqualTo(valueExpr, value);
    }

    /**
     * Creates a condition that matches when the column value equals the given number.
     *
     * @param value the value to match
     * @return the condition
     */
    public Condition equalTo(Number value) {
      return buildEqualToNumber(valueExpr, value);
    }

    /**
     * Creates a condition that matches when the column value equals the given string, ignoring
     * case.
     *
     * @param value the value to match
     * @return the condition
     */
    public Condition equalToIgnoreCase(String value) {
      return buildEqualToIgnoreCase(valueExpr, value);
    }

    /**
     * Creates a condition that matches when the column value is one of the given strings.
     *
     * @param values the values to match
     * @return the condition
     */
    public Condition in(String... values) {
      return buildIn(valueExpr, false, values);
    }

    /**
     * Creates a condition that matches when the column value is one of the given strings, ignoring
     * case.
     *
     * @param values the values to match
     * @return the condition
     */
    public Condition inIgnoreCase(String... values) {
      return buildIn(valueExpr, true, values);
    }

    /**
     * Creates a condition that matches when the column value is greater than the given number.
     *
     * @param value the threshold
     * @return the condition
     */
    public Condition greaterThan(Number value) {
      return buildCompare(valueExpr, ">", value);
    }

    /**
     * Creates a condition that matches when the column value is greater than or equal to the given
     * number.
     *
     * @param value the threshold
     * @return the condition
     */
    public Condition greaterThanOrEqual(Number value) {
      return buildCompare(valueExpr, ">=", value);
    }

    /**
     * Creates a condition that matches when the column value is less than the given number.
     *
     * @param value the threshold
     * @return the condition
     */
    public Condition lessThan(Number value) {
      return buildCompare(valueExpr, "<", value);
    }

    /**
     * Creates a condition that matches when the column value is less than or equal to the given
     * number.
     *
     * @param value the threshold
     * @return the condition
     */
    public Condition lessThanOrEqual(Number value) {
      return buildCompare(valueExpr, "<=", value);
    }

    /**
     * Creates a condition that matches when the column value is between the given min and max
     * (inclusive).
     *
     * @param min the minimum value
     * @param max the maximum value
     * @return the condition
     */
    public Condition between(Number min, Number max) {
      return buildBetween(valueExpr, min, max);
    }

    /**
     * Creates a condition that matches when the column value is null, undefined, or empty.
     *
     * @return the condition
     */
    public Condition isEmpty() {
      return buildIsNull(valueExpr);
    }

    /**
     * Creates a condition that matches when the column value is true.
     *
     * @return the condition
     */
    public Condition isTrue() {
      return buildIsTrue(valueExpr);
    }

    /**
     * Creates a condition that matches when the column value is false.
     *
     * @return the condition
     */
    public Condition isFalse() {
      return buildIsFalse(valueExpr);
    }

    /**
     * Creates a condition that matches when the column value contains the given text.
     *
     * @param text the text to search for
     * @return the condition
     */
    public Condition contains(String text) {
      return buildContains(valueExpr, text);
    }

    /**
     * Creates a condition that matches when the column value contains the given text, ignoring
     * case.
     *
     * @param text the text to search for
     * @return the condition
     */
    public Condition containsIgnoreCase(String text) {
      return buildContainsIgnoreCase(valueExpr, text);
    }
  }

  private static Condition buildEqualTo(String valueExpr, String value) {
    return new Condition("String(" + valueExpr + ") === '" + escape(value) + "'");
  }

  private static Condition buildEqualToIgnoreCase(String valueExpr, String value) {
    return new Condition(
        "String(" + valueExpr + ").toLowerCase() === '" + escape(value).toLowerCase() + "'");
  }

  private static Condition buildEqualToNumber(String valueExpr, Number value) {
    return new Condition("Number(" + valueExpr + ") === " + value);
  }

  private static Condition buildIn(String valueExpr, boolean ignoreCase, String... values) {
    StringBuilder sb = new StringBuilder("[");
    for (int i = 0; i < values.length; i++) {
      if (i > 0) {
        sb.append(",");
      }

      String escaped = escape(values[i]);
      sb.append("'").append(ignoreCase ? escaped.toLowerCase() : escaped).append("'");
    }

    sb.append("].indexOf(")
        .append(
            ignoreCase ? "String(" + valueExpr + ").toLowerCase()" : "String(" + valueExpr + ")")
        .append(") !== -1");

    return new Condition(sb.toString());
  }

  private static Condition buildCompare(String valueExpr, String op, Number value) {
    return new Condition("Number(" + valueExpr + ") " + op + " " + value);
  }

  private static Condition buildBetween(String valueExpr, Number min, Number max) {
    return new Condition(
        "Number(" + valueExpr + ") >= " + min + " && Number(" + valueExpr + ") <= " + max);
  }

  private static Condition buildIsNull(String valueExpr) {
    return new Condition(
        valueExpr + " == null || " + valueExpr + " === '' || " + valueExpr + " === undefined");
  }

  private static Condition buildIsTrue(String valueExpr) {
    return new Condition(valueExpr + " === true || " + valueExpr + " === 'true'");
  }

  private static Condition buildIsFalse(String valueExpr) {
    return new Condition(valueExpr + " === false || " + valueExpr + " === 'false'");
  }

  private static Condition buildContains(String valueExpr, String text) {
    return new Condition("String(" + valueExpr + ").indexOf('" + escape(text) + "') !== -1");
  }

  private static Condition buildContainsIgnoreCase(String valueExpr, String text) {
    return new Condition("String(" + valueExpr + ").toLowerCase().indexOf('"
        + escape(text).toLowerCase() + "') !== -1");
  }

  private static String escape(String value) {
    return value.replace("\\", "\\\\").replace("'", "\\'");
  }
}
