package com.webforj.component.table.renderer;

import java.util.Currency;
import java.util.Locale;

/**
 * A renderer that formats the column's value function result as a currency amount using the
 * browser's {@code Intl.NumberFormat}. The column should return a numeric value.
 *
 * <p>
 * The locale determines the formatting rules (grouping, decimal separator, symbol placement) and
 * the currency code is derived from the locale using {@link Currency#getInstance(Locale)}.
 * </p>
 *
 * <pre>{@code
 * CurrencyRenderer<MusicRecord> renderer = new CurrencyRenderer<>(Locale.US);
 *
 * table.addColumn("retail", MusicRecord::getRetail).setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
public class CurrencyRenderer<T> extends Renderer<T> {
  private Locale locale;
  private Currency currency;

  /**
   * Creates a new currency renderer with the given locale.
   *
   * @param locale the locale for formatting
   */
  public CurrencyRenderer(Locale locale) {
    setLocale(locale);
  }

  /**
   * Creates a new currency renderer with the given locale and currency override.
   *
   * @param locale the locale for formatting
   * @param currency the currency to use instead of the locale's default
   */
  public CurrencyRenderer(Locale locale, Currency currency) {
    this.locale = locale;
    setCurrency(currency);
  }

  /**
   * Creates a new currency renderer using the default locale.
   */
  public CurrencyRenderer() {
    this(Locale.getDefault());
  }

  /**
   * Sets the locale for formatting.
   *
   * <p>
   * The currency is automatically derived from the locale using
   * {@link Currency#getInstance(Locale)}.
   * </p>
   *
   * @param locale the locale
   * @return this renderer
   */
  public CurrencyRenderer<T> setLocale(Locale locale) {
    this.locale = locale;
    this.currency = Currency.getInstance(locale);
    fireChangeEvent();
    return this;
  }

  /**
   * Returns the locale used for formatting.
   *
   * @return the locale
   */
  public Locale getLocale() {
    return locale;
  }

  /**
   * Sets the currency, overriding the one derived from the locale.
   *
   * @param currency the currency
   * @return this renderer
   */
  public CurrencyRenderer<T> setCurrency(Currency currency) {
    this.currency = currency;
    fireChangeEvent();
    return this;
  }

  /**
   * Returns the currency.
   *
   * @return the currency
   */
  public Currency getCurrency() {
    return currency;
  }

  @Override
  public String build() {
    String languageTag = locale.toLanguageTag();
    String currencyCode = currency.getCurrencyCode();

    String key = "__cfmt_" + getKey();

    String init = "<% if (!window.KEY) {" + " window.KEY = new Intl.NumberFormat('LANG',"
        + " { style: 'currency', currency: 'CUR' }); } %>";
    String span = "<span style='display:block;text-align:right;"
        + "font-variant-numeric:tabular-nums'>" + "<%= window.KEY.format(cell.value) %></span>";

    return init.replace("KEY", key).replace("LANG", languageTag).replace("CUR", currencyCode)
        + span.replace("KEY", key);
  }
}
