package com.webforj.component.field.masked;

import com.basis.bbj.proxies.sysgui.BBjInputD;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import com.webforj.App;
import com.webforj.MaskDecorator;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.window.Window;
import com.webforj.concern.HasPattern;
import com.webforj.data.transformation.transformer.JulianLocaleDateTransformer;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.time.LocalDate;
import java.util.Locale;

/**
 * Represents a masked number field.
 *
 * <p>
 * webforJ recognizes several format indicators that all begin with a "%", followed by a letter
 * indicating which component of the date to insert:
 * </p>
 *
 * <table border="1">
 * <caption>Format Indicators</caption>
 * <tr>
 * <th>Format</th>
 * <th>Description</th>
 * </tr>
 * <tr>
 * <td>%Y</td>
 * <td>Year</td>
 * </tr>
 * <tr>
 * <td>%M</td>
 * <td>Month</td>
 * </tr>
 * <tr>
 * <td>%D</td>
 * <td>Day</td>
 * </tr>
 * </table>
 *
 * <p>
 * An optional modifier can follow format indicators to describe more specific information:
 * </p>
 *
 * <table border="1">
 * <caption>Modifiers</caption>
 * <tr>
 * <th>Modifier</th>
 * <th>Description</th>
 * </tr>
 * <tr>
 * <td>z</td>
 * <td>Zero-fill</td>
 * </tr>
 * <tr>
 * <td>s</td>
 * <td>Short text</td>
 * </tr>
 * <tr>
 * <td>l</td>
 * <td>Long text</td>
 * </tr>
 * <tr>
 * <td>p</td>
 * <td>Packed number</td>
 * </tr>
 * <tr>
 * <td>d</td>
 * <td>Decimal (default format)</td>
 * </tr>
 * </table>
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
public class MaskedDateField extends DwcMaskedField<MaskedDateField, LocalDate>
    implements HasPattern<MaskedDateField> {
  static final String DEFAULT_MASK = "%Dz-%Mz-%Yl";
  private final JulianLocaleDateTransformer transformer = new JulianLocaleDateTransformer();
  private Locale locale;
  private String pattern;
  private LocalDate value;
  private boolean visibleCalendarIcon = false;
  private boolean toggleCalendarOnEnter = false;
  private boolean openCalendarInvoked = false;
  private boolean showCalendarWeeks = false;

  /**
   * Constructs a new masked date field.
   */
  public MaskedDateField() {
    super();

    setMask(DEFAULT_MASK);
    setVisibleCalendarIcon(true);
    setToggleCalendarOnEnter(true);
    setLocale(App.getLocale());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getText() {
    if (isAttached()) {
      return super.getText();
    }

    String ret = value == null ? "" : MaskDecorator.forDate(value, getMask());
    return ret == null ? "" : ret;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedDateField setValue(LocalDate value) {
    this.value = value;
    BBjInputD field = inferDateField();

    if (field != null) {
      try {
        int julian = transformer.transformToComponent(value);
        field.setValue(BasisNumber.valueOf(julian));
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public LocalDate getValue() {
    BBjInputD field = inferDateField();

    if (field != null) {
      try {
        int julian = field.getValue().intValue();
        return transformer.transformToModel(julian);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    } else {
      // if there no value, try to parse the text in case the field is not attached
      if (value == null) {
        LocalDate parsed = convertValue(getText());
        if (parsed != null) {
          return parsed;
        }
      }
    }

    return value;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getMaskedValue() {
    LocalDate theValue = getValue();
    if (theValue == null) {
      return "";
    }

    return MaskDecorator.forDate(theValue, getMask());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedDateField setPattern(String pattern) {
    this.pattern = pattern;
    setUnrestrictedProperty("pattern", pattern);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getPattern() {
    return pattern;
  }

  /**
   * Sets the locale for the date field.
   *
   * <p>
   * The locale is used to format and parse the date in the field.
   * </p>
   *
   * @param locale the locale to set.
   * @return this component itself.
   */
  public MaskedDateField setLocale(Locale locale) {
    this.locale = locale;

    BBjInputD field = inferDateField();

    if (field != null) {
      field.setLocale(locale.toString());
    }

    return this;
  }

  /**
   * Gets the locale of the date field.
   *
   * @return the locale of the date field.
   */
  public Locale getLocale() {
    return locale;
  }

  /**
   * Sets the visibility of the calendar icon.
   *
   * @param visibleCalendarIcon the visibility of the calendar icon.
   * @return this component itself.
   */
  public MaskedDateField setVisibleCalendarIcon(boolean visibleCalendarIcon) {
    this.visibleCalendarIcon = visibleCalendarIcon;
    setUnrestrictedProperty("visibleCalendarIcon", visibleCalendarIcon);
    return this;
  }

  /**
   * Gets the visibility of the calendar icon.
   *
   * @return the visibility of the calendar icon.
   */
  public boolean isVisibleCalendarIcon() {
    return visibleCalendarIcon;
  }

  /**
   * Sets whether the calendar should be toggled on enter.
   *
   * @param toggleCalendarOnEnter whether the calendar should be toggled on enter.
   * @return this component itself.
   */
  public MaskedDateField setToggleCalendarOnEnter(boolean toggleCalendarOnEnter) {
    this.toggleCalendarOnEnter = toggleCalendarOnEnter;
    setUnrestrictedProperty("toggleCalendarOnEnter", toggleCalendarOnEnter);
    return this;
  }

  /**
   * Gets whether the calendar should be toggled on enter.
   *
   * @return whether the calendar should be toggled on enter.
   */
  public boolean isToggleCalendarOnEnter() {
    return toggleCalendarOnEnter;
  }

  /**
   * Opens the calendar.
   */
  public MaskedDateField openCalendar() {
    this.openCalendarInvoked = true;

    BBjInputD field = inferDateField();
    if (field != null) {
      field.calendar();
    }

    return this;
  }

  /**
   * Alias for {@link #openCalendar()}.
   */
  public MaskedDateField showCalendar() {
    return openCalendar();
  }

  /**
   * Sets whether the calendar should show weeks.
   *
   * @param showWeeks whether the calendar should show weeks.
   * @return this component itself.
   */
  public MaskedDateField setShowCalendarWeeks(boolean showWeeks) {
    this.showCalendarWeeks = showWeeks;

    BBjInputD field = inferDateField();
    try {
      if (field != null) {
        field.setShowWeeks(showWeeks);
      }
    } catch (BBjException e) {
      throw new WebforjRuntimeException(e);
    }

    return this;
  }

  /**
   * Gets whether the calendar should show weeks.
   *
   * @return whether the calendar should show weeks.
   */
  public boolean isShowCalendarWeeks() {
    return showCalendarWeeks;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected LocalDate convertValue(String value) {
    return MaskDecorator.parseDate(value, getMask(), getLocale());
  }

  @Override
  protected void onAttach() {
    super.onAttach();

    if (locale != null) {
      setLocale(locale);
    }

    // value has higher priority than text
    if (value != null) {
      setValue(value);
    }

    if (showCalendarWeeks) {
      setShowCalendarWeeks(showCalendarWeeks);
    }

    if (openCalendarInvoked) {
      openCalendar();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      byte[] flags = BBjFunctionalityHelper.buildStandardCreationFlags(isVisible(), isEnabled());
      setControl(w.addInputD(flags));
    } catch (BBjException | IllegalAccessException e) {
      throw new WebforjRuntimeException("Failed to create BBjInputD", e);
    }
  }

  private BBjInputD inferDateField() {
    return (BBjInputD) inferField();
  }
}
