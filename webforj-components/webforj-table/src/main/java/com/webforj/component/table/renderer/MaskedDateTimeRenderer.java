package com.webforj.component.table.renderer;

import com.webforj.App;
import com.webforj.component.element.annotation.NodeName;
import java.util.Locale;

/**
 * A renderer that formats the column's date, time, or datetime value using a date mask via the
 * {@code dwc-format-datetime} component.
 *
 * <pre>{@code
 * MaskedDateTimeRenderer<Employee> renderer = new MaskedDateTimeRenderer<>("%Mz/%Dz/%Yz");
 *
 * table.addColumn("hireDate", Employee::getHireDate).setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
@NodeName("dwc-format-datetime")
public class MaskedDateTimeRenderer<T> extends AbstractVoidElementRenderer<T> {
  static final String DEFAULT_MASK = "%Mz/%Dz/%Yl";

  private String mask;
  private Locale locale;

  /**
   * Creates a new masked datetime renderer with the given mask and locale.
   *
   * @param mask the date mask pattern
   * @param locale the locale for formatting
   */
  public MaskedDateTimeRenderer(String mask, Locale locale) {
    setContent("");
    setMask(mask);
    setLocale(locale);
  }

  /**
   * Creates a new masked datetime renderer with the given mask using the application locale.
   *
   * @param mask the date mask pattern
   */
  public MaskedDateTimeRenderer(String mask) {
    this(mask, App.getLocale());
  }

  /**
   * Creates a new masked datetime renderer with the default mask using the application locale.
   */
  public MaskedDateTimeRenderer() {
    this(DEFAULT_MASK);
  }

  /**
   * Sets the date mask pattern.
   *
   * @param mask the mask pattern (e.g. {@code "%Mz/%Dz/%Yz"}, {@code "%Hz:%mz"})
   * @return this renderer
   */
  public MaskedDateTimeRenderer<T> setMask(String mask) {
    this.mask = mask;
    setAttribute("mask", mask);
    return this;
  }

  /**
   * Returns the date mask pattern.
   *
   * @return the mask pattern
   */
  public String getMask() {
    return mask;
  }

  /**
   * Sets the locale for formatting.
   *
   * @param locale the locale
   * @return this renderer
   */
  public MaskedDateTimeRenderer<T> setLocale(Locale locale) {
    this.locale = locale;
    setAttribute("locale", locale.toLanguageTag());
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

  @Override
  public String build() {
    setAttribute("value", "<%= cell.value %>", false);
    return super.build();
  }
}
