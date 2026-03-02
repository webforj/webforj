package com.webforj.component.table.renderer;

/**
 * A renderer that displays a placeholder when the cell value is null or empty, and the column's
 * value function result otherwise.
 *
 * <pre>{@code
 * NullRenderer<MusicRecord> renderer = new NullRenderer<>();
 * renderer.setPlaceholder("N/A");
 *
 * table.addColumn("binLocation", MusicRecord::getBinLocation).setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
public class NullRenderer<T> extends Renderer<T> {
  static final String DEFAULT_PLACEHOLDER = "\u2014";
  private String placeholder = DEFAULT_PLACEHOLDER;

  /**
   * Creates a new null renderer with the given placeholder text.
   *
   * @param placeholder the placeholder text shown when the value is null or empty
   */
  public NullRenderer(String placeholder) {
    setPlaceholder(placeholder);
  }

  /**
   * Creates a new null renderer.
   */
  public NullRenderer() {
    this(DEFAULT_PLACEHOLDER);
  }

  /**
   * Sets the placeholder text shown when the value is null or empty.
   *
   * @param placeholder the placeholder text
   * @return this renderer
   */
  public NullRenderer<T> setPlaceholder(String placeholder) {
    this.placeholder = placeholder;
    fireChangeEvent();
    return this;
  }

  /**
   * Returns the placeholder text.
   *
   * @return the placeholder text
   */
  public String getPlaceholder() {
    return placeholder;
  }

  /** {@inheritDoc} */
  @Override
  public String build() {
    String escaped = placeholder.replace("'", "\\'");
    return "<% if (cell.value == null || cell.value === '' || cell.value === undefined"
        + " || cell.value === 'null') { %>" + "<span>" + escaped + "</span>" + "<% } else { %>"
        + "<%= cell.value %>" + "<% } %>";
  }
}
