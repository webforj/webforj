package com.webforj.component.field;

import com.basis.bbj.funcs.Day;
import com.basis.bbj.proxies.sysgui.BBjInputD;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import com.webforj.App;
import com.webforj.MaskDecorator;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.window.Window;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.data.transformation.transformer.JulianLocaleDateTransformer;
import com.webforj.dispatcher.EventListener;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.time.LocalDate;
import java.time.Month;
import java.util.Arrays;
import java.util.List;

/**
 * Represents a masked date field.
 *
 * <p>
 * The masked date field is a text field that allows the user to enter dates as numbers and
 * automatically formats them according to a mask when the user leaves the field. The mask is a
 * string that contains the characters that define the format of the date and the field will
 * interpret dates based on the mask. The resulting a human-readable string representing the date.
 * </p>
 *
 * <p>
 * There are three basic date formats in use around the world, based on the order of the Month, Day,
 * and Year. Within these basic formats, there are local differences in the preferred separator,
 * (usually "-", "/" or "."), whether years are shown as four digits or two digits, and whether
 * 1-digit month and day numbers are padded to two digits with a leading "0"). In Europe, most
 * countries use day/month/year format. The United States uses month/day/year. China, Japan, and
 * Korea use year/month/day (the ISO standard, normally formatted as YYYY-MM-DD).
 * </p>
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
 * <h3>Date Parser</h3>
 *
 * <p>
 * The date parser interprets date strings based on the date mask. For example, if the date mask is
 * %Mz/%Dz/%Yz (US format, the webforJ default setting), the date is parsed with the understanding
 * that it is in month/day/year order. Dates are accepted in all reasonable numeric formats, with or
 * without delimiters. For example, assuming that today is September 15, 2012, this is how various
 * inputs would be interpreted:
 * </p>
 *
 * <table>
 * <tbody>
 * <tr>
 * <td>
 * <p>
 * Entry
 * </p>
 * </td>
 * <td>
 * <p>
 * YMD (ISO)
 * </p>
 * </td>
 * <td>
 * <p>
 * MDY (US)
 * </p>
 * </td>
 * <td>
 * <p>
 * DMY (EU)
 * </p>
 * </td>
 * </tr>
 * <tr>
 * <td>
 * <p>
 * 1
 * </p>
 * </td>
 * <td colspan="3">
 * <p>
 * A single digit is always interpreted as a day number within the current month, so this would be
 * September 1, 2012.
 * </p>
 * </td>
 * </tr>
 * <tr>
 * <td>
 * <p>
 * 12
 * </p>
 * </td>
 * <td colspan="3">
 * <p>
 * Two digits are always interpreted as a day number within the current month, so this would be
 * September 12, 2012.
 * </p>
 * </td>
 * </tr>
 * <tr>
 * <td>
 * <p>
 * 112
 * </p>
 * </td>
 * <td colspan="2">
 * <p>
 * Three digits are interpreted as a 1-digit month number followed by a 2-digit day number, so this
 * would be January 12, 2012.
 * </p>
 * </td>
 * <td>
 * <p>
 * Three digits are interpreted as a 1-digit day number followed by a two-digit month number, so
 * this would be 1 December 2012.
 * </p>
 * </td>
 * </tr>
 * <tr>
 * <td>
 * <p>
 * 1004
 * </p>
 * </td>
 * <td colspan="2">
 * <p>
 * Four digits are interpreted as MMDD, so this would be October 4, 2012.
 * </p>
 * </td>
 * <td>
 * <p>
 * Four digits are interpreted as DDMM, so this would be 10 April 2012.
 * </p>
 * </td>
 * </tr>
 * <tr>
 * <td>
 * <p>
 * 020304
 * </p>
 * </td>
 * <td>
 * <p>
 * Six digits are interpreted as YYMMDD, so this would be March 4, 2002.
 * </p>
 * </td>
 * <td>
 * <p>
 * Six digits are interpreted as MMDDYY, so this would be February 3, 2004.
 * </p>
 * </td>
 * <td>
 * <p>
 * Six digits are interpreted as DDMMYY, so this would be 2 March 2004.
 * </p>
 * </td>
 * </tr>
 * <tr>
 * <td>
 * <p>
 * 8 digits
 * </p>
 * </td>
 * <td>
 * <p>
 * Eight digits are interpreted as YYYYMMDD. For example, 20040612 is June 12, 2004.
 * </p>
 * </td>
 * <td>
 * <p>
 * Eight digits are interpreted as MMDDYYYY. For example, 06122004 is June 12, 2004.
 * </p>
 * </td>
 * <td>
 * <p>
 * Eight digits are interpreted as DDMMYYYY. For example, 06122004 is 6 December 2004.
 * </p>
 * </td>
 * </tr>
 * <tr>
 * <td>
 * <p>
 * 12/6
 * </p>
 * </td>
 * <td colspan="2">
 * <p>
 * Two numbers separated by any valid delimiter is interpreted as MM/DD, so this would be December
 * 6, 2012. (Note: All characters except for letters and digits are considered valid delimiters.)
 * </p>
 * </td>
 * <td>
 * <p>
 * Two numbers separated by any delimiter is interpreted as DD/MM, so this would be 12 June 2012.
 * </p>
 * </td>
 * </tr>
 * <tr>
 * <td>
 * <p>
 * 3/4/5
 * </p>
 * </td>
 * <td>
 * <p>
 * April 5, 2012
 * </p>
 * </td>
 * <td>
 * <p>
 * March 4, 2005
 * </p>
 * </td>
 * <td>
 * <p>
 * 3 April 2005
 * </p>
 * </td>
 * </tr>
 * </tbody>
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
// @formatter:off
public sealed class MaskedDateField extends DwcDateTimeMaskedField<MaskedDateField, LocalDate>
    permits MaskedDateFieldSpinner {
  // @formatter:on
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
  private boolean textualDateParsing = false;

  /**
   * Constructs a new masked field with a label, value, and placeholder.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param placeholder the placeholder of the field
   */
  public MaskedDateField(String label, LocalDate value, String placeholder) {
    super(label, value, placeholder);
  }

  /**
   * Constructs a new masked field with a label, value, and a value change listener.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param listener the value change listener
   */
  public MaskedDateField(String label, LocalDate value,
      EventListener<ValueChangeEvent<LocalDate>> listener) {
    super(label, value, listener);
    postInit();
  }

  /**
   * Constructs a new masked field with a label and value.
   *
   * @param label the label of the field
   * @param value the value of the field
   */
  public MaskedDateField(String label, LocalDate value) {
    super(label, value);
    postInit();
  }

  /**
   * Constructs a new masked field with a label and a value change listener.
   *
   * @param label the label of the field
   * @param listener the value change listener
   */
  public MaskedDateField(String label, EventListener<ValueChangeEvent<LocalDate>> listener) {
    super(label, listener);
    postInit();
  }

  /**
   * Constructs a new masked field with a value change listener.
   *
   * @param listener the value change listener
   */
  public MaskedDateField(EventListener<ValueChangeEvent<LocalDate>> listener) {
    super(listener);
    postInit();
  }

  /**
   * Constructs a new masked field with a label.
   *
   * @param label the label of the field
   */
  public MaskedDateField(String label) {
    super(label);
    postInit();
  }

  /**
   * Constructs a new masked field.
   */
  public MaskedDateField() {
    super();
    postInit();
  }

  private void postInit() {
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
   * Gets the date picker associated with this field.
   *
   * @return the date picker.
   */
  public DatePicker getPicker() {
    return picker;
  }

  /**
   * Sets whether to enable textual date parsing.
   *
   * <p>
   * When enabled, month and day names can be used in date input according to the mask.
   * </p>
   *
   * <p>
   * <b>Month Name Substitution (%Ms, %Ml):</b><br>
   * Month names replace numeric months in the input. Works in any position:
   * <ul>
   * <li>Mask "%Ms/%Dz/%Yz" accepts "Sep/01/25" → valid Date</li>
   * <li>Mask "%Ml/%Dz/%Yz" accepts "September/01/25" → valid Date</li>
   * <li>Mask "%Dz/%Ml/%Yz" accepts "01/September/25" → valid Date</li>
   * <li>Numeric fallback: "09/01/25" still works with "%Ms/%Dz/%Yz"</li>
   * </ul>
   * Supports all 12 months in both short (Jan, Feb...) and long (January, February...) forms.
   * </p>
   *
   * <p>
   * <b>Day Name Decoration (%Ds, %Dl):</b><br>
   * Day-of-week names can be included but are stripped during parsing. The mask <b>MUST include %Dz
   * or %Dd</b>:
   * <ul>
   * <li>Mask "%Ds %Mz/%Dz/%Yz" accepts "Mon 09/01/25" → valid Date</li>
   * <li>Mask "%Dl %Mz/%Dz/%Yz" accepts "Monday 09/01/25" → valid Date</li>
   * <li>Mask "%Mz/%Dz/%Yz %Ds" accepts "09/01/25 Tue" → valid Date</li>
   * <li>Mask "%Dl/%Mz/%Yz" with "Monday/09/25" → <b>invalid Date</b> (missing %Dz)</li>
   * <li>Mask "%Mz/%Dl/%Yz" with "09/Monday/25" → <b>invalid Date</b> (missing %Dz)</li>
   * </ul>
   * Supports all 7 weekdays in both short (Mon, Tue...) and long (Monday, Tuesday...) forms.
   * </p>
   *
   * <p>
   * <b>Additional Parsing Rules:</b>
   * <ul>
   * <li>Case-insensitive: "MONDAY 09/01/25" works same as "monday 09/01/25"</li>
   * <li>Locale-aware: "septembre/01/25" (French) or "Montag 09/01/25" (German)</li>
   * </ul>
   * </p>
   *
   * <p>
   * <b>When Disabled (default):</b><br>
   * Mask "%Ms/%Dz/%Yz" with "Sep/01/25" → <b>invalid Date</b> (names not recognized)<br>
   * Only numeric input accepted: "09/01/25" → valid Date
   * </p>
   *
   * @param textualDateParsing true to enable month/day name parsing, false for numeric only
   * @return this masked date field
   * @since 25.10
   */
  public MaskedDateField setTextualDateParsing(boolean textualDateParsing) {
    this.textualDateParsing = textualDateParsing;
    setUnrestrictedProperty("textualDateParsing", textualDateParsing);
    return this;
  }

  /**
   * Gets whether textual date parsing is enabled.
   *
   * @return true if textual date parsing is enabled, false otherwise
   * @since 25.10
   */
  public boolean isTextualDateParsing() {
    return textualDateParsing;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected LocalDate convertValue(String value) {
    return MaskDecorator.parseDate(value, getMask(), getLocale());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> getRestrictedProperties() {
    List<String> properties = super.getRestrictedProperties();
    properties.addAll(Arrays.asList("autoValidate", "autoValidateOnLoad", "autoWasValidated",
        "autocomplete", "autocorrect", "autofocus", "calendarAutoOpen", "calendarIconVisible",
        "calendarPosition", "customValue", "disabled", "expanse", "hasFocus", "highlightBehaviors",
        "insertMode", "invalid", "invalidMessage", "julianValue", "label", "locale", "mask", "max",
        "maxlength", "min", "name", "pattern", "placeholder", "readonly", "required",
        "restoreValue", "showSpinners", "showWeeks", "spellcheck", "spinnable", "tabTraversable",
        "toggleCalendarOnEnter", "valid", "validationIcon", "validationPopoverDistance",
        "validationPopoverPlacement", "validationPopoverSkidding", "validationStyle", "validator",
        "value", "visibleCalendarIcon"));

    return properties;
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
      setControl(w.addInputD(resolveControlId(w), flags));
    } catch (BBjException | IllegalAccessException e) {
      throw new WebforjRuntimeException("Failed to create BBjInputD", e);
    }
  }

  private BBjInputD inferDateField() {
    return (BBjInputD) inferField();
  }

  /**
   * Represents the MaskedDateField date picker.
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
