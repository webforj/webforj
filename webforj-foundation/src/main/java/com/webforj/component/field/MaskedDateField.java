package com.webforj.component.field;

import com.basis.bbj.proxies.sysgui.BBjInputD;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import com.webforj.App;
import com.webforj.MaskDecorator;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.window.Window;
import com.webforj.data.transformation.transformer.JulianLocaleDateTransformer;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.time.LocalDate;

/**
 * Represents a masked date field.
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
public sealed class MaskedDateField extends DwcDateTimeMaskedField<MaskedDateField, LocalDate>
    permits MaskedDateFieldSpinner {
  static final String DEFAULT_MASK = "%Mz/%Dz/%Yl";
  private final JulianLocaleDateTransformer transformer = new JulianLocaleDateTransformer();
  private LocalDate value;
  private LocalDate min;
  private LocalDate max;
  private boolean pickerIconVisible = false;
  private boolean pickerOpenInvoked = false;
  private boolean pickerShowWeeks = false;
  private boolean pickerAutoOpen = false;
  private final DatePicker picker = new DatePicker();

  /**
   * Constructs a new masked date field.
   */
  public MaskedDateField() {
    super();

    setMask(DEFAULT_MASK);
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
  public MaskedDateField setMin(LocalDate min) {
    this.min = min;
    int julian = transformer.transformToComponent(min);
    setUnrestrictedProperty("min", julian);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public LocalDate getMin() {
    return min;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedDateField setMax(LocalDate max) {
    this.max = max;
    int julian = transformer.transformToComponent(max);
    setUnrestrictedProperty("max", julian);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public LocalDate getMax() {
    return max;
  }

  /**
   * Gets the date picker.
   *
   * @return the date picker.
   */
  public DatePicker getPicker() {
    return picker;
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

    // value has higher priority than text
    if (value != null) {
      setValue(value);
    }

    if (pickerShowWeeks) {
      getPicker().setShowWeeks(pickerShowWeeks);
    }

    if (pickerOpenInvoked) {
      getPicker().open();
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

  /**
   * Represents the date picker.
   *
   * <p>
   * The date picker is a calendar that allows the user to select a date.
   * </p>
   *
   * @author Hyyan Abo Fakher
   * @since 24.10
   */
  public final class DatePicker {

    private DatePicker() {
      setIconVisible(true);
    }

    /**
     * Sets the visibility of the date picker icon.
     *
     * @param iconVisible the visibility of the picker icon.
     * @return this date picker.
     */
    public DatePicker setIconVisible(boolean iconVisible) {
      pickerIconVisible = iconVisible;
      setUnrestrictedProperty("visibleCalendarIcon", iconVisible);

      return this;
    }

    /**
     * Gets the visibility of the picker icon.
     *
     * @return the visibility of the picker icon.
     */
    public boolean isIconVisible() {
      return pickerIconVisible;
    }

    /**
     * Sets whether the picker should open automatically when the user start interacting with the
     * component by click, pressing Enter or arrow keys.
     *
     * @param autoOpen whether the picker should open automatically.
     * @return this date picker.
     */
    public DatePicker setAutoOpen(boolean autoOpen) {
      MaskedDateField.this.pickerAutoOpen = autoOpen;
      setUnrestrictedProperty("calendarAutoOpen", autoOpen);

      return this;
    }

    /**
     * Gets whether the picker should open automatically when the user start interacting with the
     * component by click, pressing Enter or arrow keys.
     *
     * @return whether the picker should open automatically.
     */
    public boolean isAutoOpen() {
      return pickerAutoOpen;
    }

    /**
     * Sets whether the picker should show weeks.
     *
     * @param showWeeks whether the picker should show weeks.
     * @return this date picker.
     */
    public DatePicker setShowWeeks(boolean showWeeks) {
      pickerShowWeeks = showWeeks;

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
    public boolean isShowWeeks() {
      return pickerShowWeeks;
    }

    /**
     * Opens the calendar.
     */
    public DatePicker open() {
      pickerOpenInvoked = true;
      BBjInputD field = inferDateField();
      if (field != null) {
        field.calendar();
      }

      return this;
    }

    /**
     * Alias for {@link #open()}.
     */
    public DatePicker show() {
      return open();
    }
  }
}
