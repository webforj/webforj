package com.webforj.component.table.renderer;

import com.webforj.App;
import com.webforj.component.element.annotation.NodeName;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

/**
 * A renderer that formats the column's numeric value using a number mask via the
 * {@code dwc-format-number} component.
 *
 * <pre>{@code
 * MaskedNumberRenderer<MusicRecord> renderer = new MaskedNumberRenderer<>("###,##0.00");
 *
 * table.addColumn("cost", MusicRecord::getCost).setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
@NodeName("dwc-format-number")
public class MaskedNumberRenderer<T> extends AbstractVoidElementRenderer<T> {
  static final String DEFAULT_MASK = "-########";

  private String mask;
  private Locale locale;

  /**
   * Creates a new masked number renderer with the given mask and locale.
   *
   * @param mask the number mask pattern
   * @param locale the locale for deriving separator characters
   */
  public MaskedNumberRenderer(String mask, Locale locale) {
    setContent("");
    setMask(mask);
    setLocale(locale);
  }

  /**
   * Creates a new masked number renderer with the given mask using the application locale.
   *
   * @param mask the number mask pattern
   */
  public MaskedNumberRenderer(String mask) {
    this(mask, App.getLocale());
  }

  /**
   * Creates a new masked number renderer using the application locale.
   */
  public MaskedNumberRenderer() {
    this(DEFAULT_MASK);
  }

  /**
   * Sets the locale. The grouping and decimal separator characters are derived from the locale.
   *
   * @param locale the locale
   * @return this renderer
   */
  public MaskedNumberRenderer<T> setLocale(Locale locale) {
    this.locale = locale;
    DecimalFormatSymbols symbols = new DecimalFormatSymbols(locale);
    setAttribute("group-separator", String.valueOf(symbols.getGroupingSeparator()));
    setAttribute("decimal-separator", String.valueOf(symbols.getDecimalSeparator()));
    return this;
  }

  /**
   * Returns the locale.
   *
   * @return the locale
   */
  public Locale getLocale() {
    return locale;
  }

  /**
   * Sets the number mask pattern.
   *
   * @param mask the mask pattern (e.g. {@code "###,##0.00"})
   * @return this renderer
   */
  public MaskedNumberRenderer<T> setMask(String mask) {
    this.mask = mask;
    setAttribute("mask", mask);
    return this;
  }

  /**
   * Returns the number mask pattern.
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
