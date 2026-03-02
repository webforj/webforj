package com.webforj.component.table.renderer;

import com.webforj.component.element.annotation.NodeName;

/**
 * A renderer that formats the column's string value using a string mask via the
 * {@code dwc-format-text} component.
 *
 * <pre>{@code
 * MaskedTextRenderer<Customer> renderer = new MaskedTextRenderer<>("(###) ###-####");
 *
 * table.addColumn("phone", Customer::getPhone).setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
@NodeName("dwc-format-text")
public class MaskedTextRenderer<T> extends AbstractVoidElementRenderer<T> {
  static final String DEFAULT_MASK =
      "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
          + "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
          + "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
          + "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";

  private String mask;

  /**
   * Creates a new masked text renderer with the given mask.
   *
   * @param mask the string mask pattern
   */
  public MaskedTextRenderer(String mask) {
    setContent("");
    setMask(mask);
  }

  /**
   * Creates a new masked text renderer with the default mask.
   */
  public MaskedTextRenderer() {
    this(DEFAULT_MASK);
  }

  /**
   * Sets the string mask pattern.
   *
   * @param mask the mask pattern (e.g. {@code "(###) ###-####"})
   * @return this renderer
   */
  public MaskedTextRenderer<T> setMask(String mask) {
    this.mask = mask;
    setAttribute("mask", mask);
    return this;
  }

  /**
   * Returns the string mask pattern.
   *
   * @return the mask pattern
   */
  public String getMask() {
    return mask;
  }

  @Override
  public String build() {
    setAttribute("value", "<%= cell.value %>", false);
    return super.build();
  }
}
