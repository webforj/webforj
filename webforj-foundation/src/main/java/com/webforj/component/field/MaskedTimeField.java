package com.webforj.component.field;

import com.basis.bbj.proxies.sysgui.BBjInputT;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import com.webforj.App;
import com.webforj.MaskDecorator;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.window.Window;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.data.transformation.transformer.HoursLocalTimeTransformer;
import com.webforj.dispatcher.EventListener;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.time.Duration;
import java.time.LocalTime;
import java.util.Arrays;
import java.util.List;

/**
 * Represents a masked time field.
 *
 * <p>
 * The masked time field is a text field that allows the user to enter times as numbers and
 * automatically formats them according to a mask when the user leaves the field. The mask is a
 * string that contains the characters that define the format of the time and the field will
 * interpret times based on the mask. The resulting a human-readable string representing the time.
 * </p>
 *
 * <p>
 * webforJ recognizes several format indicators that all begin with a "%", followed by a letter
 * indicating which component of the time to insert:
 * </p>
 *
 * <table border="1">
 * <caption>Format Indicators</caption>
 * <tr>
 * <th>Format</th>
 * <th>Description</th>
 * </tr>
 * <tr>
 * <td>%H</td>
 * <td>Hour (24-hour clock)</td>
 * </tr>
 * <tr>
 * <td>%h</td>
 * <td>Hour (12-hour clock)</td>
 * </tr>
 * <tr>
 * <td>%m</td>
 * <td>Minute</td>
 * </tr>
 * <tr>
 * <td>%s</td>
 * <td>Second</td>
 * </tr>
 * <tr>
 * <td>%p</td>
 * <td>AM/PM</td>
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
public sealed class MaskedTimeField extends DwcDateTimeMaskedField<MaskedTimeField, LocalTime>
    permits MaskedTimeFieldSpinner {
  static final String DEFAULT_MASK = "%h:%mz %a";
  private final HoursLocalTimeTransformer transformer = new HoursLocalTimeTransformer();
  private LocalTime value;
  private LocalTime min;
  private LocalTime max;
  private boolean pickerIconVisible = false;
  private boolean pickerOpenInvoked = false;
  private boolean pickerAutoOpen = false;
  private String pickerType;
  private Duration pickerStep;
  private final TimePicker picker = new TimePicker();

  /**
   * Constructs a new masked field with a label, value, and placeholder.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param placeholder the placeholder of the field
   */
  public MaskedTimeField(String label, LocalTime value, String placeholder) {
    super(label, value, placeholder);
    postInit();
  }

  /**
   * Constructs a new masked field with a label, value, and a value change listener.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param listener the value change listener
   */
  public MaskedTimeField(String label, LocalTime value,
      EventListener<ValueChangeEvent<LocalTime>> listener) {
    super(label, value, listener);
    postInit();
  }

  /**
   * Constructs a new masked field with a label and value.
   *
   * @param label the label of the field
   * @param value the value of the field
   */
  public MaskedTimeField(String label, LocalTime value) {
    super(label, value);
    postInit();
  }

  /**
   * Constructs a new masked field with a label and a value change listener.
   *
   * @param label the label of the field
   * @param listener the value change listener
   */
  public MaskedTimeField(String label, EventListener<ValueChangeEvent<LocalTime>> listener) {
    super(label, listener);
    postInit();
  }

  /**
   * Constructs a new masked field with a value change listener.
   *
   * @param listener the value change listener
   */
  public MaskedTimeField(EventListener<ValueChangeEvent<LocalTime>> listener) {
    super(listener);
    postInit();
  }

  /**
   * Constructs a new masked field with a label.
   *
   * @param label the label of the field
   */
  public MaskedTimeField(String label) {
    super(label);
    postInit();
  }

  /**
   * Constructs a new masked field.
   */
  public MaskedTimeField() {
    super();
    postInit();
  }

  private void postInit() {
    setMask(DEFAULT_MASK);
    setLocale(App.getLocale());
    setTypingMode(TypingMode.INSERT);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getText() {
    if (isAttached()) {
      return super.getText();
    }

    String ret = value == null ? "" : MaskDecorator.forTime(value, getMask());
    return ret == null ? "" : ret;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedTimeField setValue(LocalTime value) {
    this.value = value;
    BBjInputT field = inferTimeField();

    if (field != null) {
      try {
        double hours = transformer.transformToComponent(value);
        field.setValue(BasisNumber.valueOf(hours));
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
  public LocalTime getValue() {
    BBjInputT field = inferTimeField();

    if (field != null) {
      try {
        double hours = field.getValue().doubleValue();
        return transformer.transformToModel(hours);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    } else {
      // if there no value, try to parse the text in case the field is not attached
      if (value == null) {
        LocalTime parsed = convertValue(getText());
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
    LocalTime theValue = getValue();
    if (theValue == null) {
      return "";
    }

    return MaskDecorator.forTime(theValue, getMask());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedTimeField setMin(LocalTime min) {
    this.min = min;
    double hours = transformer.transformToComponent(min);
    setUnrestrictedProperty("min", hours);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public LocalTime getMin() {
    return min;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedTimeField setMax(LocalTime max) {
    this.max = max;
    double hours = transformer.transformToComponent(max);
    setUnrestrictedProperty("max", hours);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public LocalTime getMax() {
    return max;
  }

  /**
   * Gets the time picker.
   *
   * @return the time picker.
   */
  public TimePicker getPicker() {
    return picker;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected LocalTime convertValue(String value) {
    return MaskDecorator.parseTime(value, getMask(), getLocale());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> getRestrictedProperties() {
    List<String> properties = super.getRestrictedProperties();
    properties.addAll(Arrays.asList("autoValidate", "autoValidateOnLoad", "autoWasValidated",
        "autofocus", "customValue", "disabled", "expanse", "hasFocus", "highlightBehaviors",
        "insertMode", "invalid", "invalidMessage", "label", "locale", "mask", "max", "min", "name",
        "pattern", "pickerAutoOpen", "pickerDistance", "pickerIconVisible", "pickerMaxRowCount",
        "pickerOpenHeight", "pickerOpenWidth", "pickerPlacement", "pickerSkidding", "pickerType",
        "placeholder", "rawValue", "readonly", "required", "restoreValue", "showSpinners",
        "spinnable", "step", "tabTraversable", "valid", "validationIcon",
        "validationPopoverDistance", "validationPopoverPlacement", "validationPopoverSkidding",
        "validationStyle", "validator", "value"));

    return properties;
  }

  @Override
  protected void onAttach() {
    super.onAttach();

    // value has higher priority than text
    if (value != null) {
      setValue(value);
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
      setControl(w.addInputT(flags));
    } catch (BBjException | IllegalAccessException e) {
      throw new WebforjRuntimeException("Failed to create BBjInputT", e);
    }
  }

  private BBjInputT inferTimeField() {
    return (BBjInputT) inferField();
  }

  /**
   * Represents a time picker.
   *
   * <p>
   * The time picker is a dropdown that allows users to select a time from a list of predefined
   * options.
   * </p>
   *
   * @author Hyyan Abo Fakher
   * @since 24.10
   */
  public final class TimePicker {

    private TimePicker() {
      setType("time");
      setIconVisible(true);
      setStep(Duration.ofHours(1));
    }

    /**
     * Sets the visibility of the picker icon.
     *
     * @param visibleIcon {@code true} to show the picker icon, {@code false} to hide it
     * @return this instance
     */
    public TimePicker setIconVisible(boolean visibleIcon) {
      pickerIconVisible = visibleIcon;
      setUnrestrictedProperty("pickerIconVisible", visibleIcon);
      return this;
    }

    /**
     * Gets the visibility of the picker icon.
     *
     * @return {@code true} if the picker icon is visible, {@code false} otherwise
     */
    public boolean isIconVisible() {
      return pickerIconVisible;
    }

    /**
     * Sets whether the picker should be automatically opened when the user start interacting with
     * the component by click, clicking Enter or arrow keys.
     *
     * @param autoOpen {@code true} to open the picker automatically, {@code false} otherwise
     * @return this instance
     */
    public TimePicker setAutoOpen(boolean autoOpen) {
      pickerAutoOpen = autoOpen;
      setUnrestrictedProperty("pickerAutoOpen", autoOpen);
      return this;
    }

    /**
     * Gets whether the picker should be automatically opened when the user start interacting with
     * the component by click, clicking Enter or arrow keys.
     *
     * @return {@code true} if the picker should be automatically opened, {@code false} otherwise
     */
    public boolean isAutoOpen() {
      return pickerAutoOpen;
    }

    /**
     * Gives the picker dropdown a custom type.
     *
     * <p>
     * You have the option to assign a custom type to the picker dropdown, and this custom type will
     * appear in the dropdown list as a DOM attribute named <code>data-dropdown-for="TYPE"</code>.
     * This attribute can be quite valuable for styling purposes.
     * </p>
     *
     * <p>
     * When you open the picker dropdown, it's taken out of its current position in the DOM and
     * relocated to the end of the page body. This detachment creates a situation where directly
     * targeting the dropdown using CSS or shadow part selectors from the parent component becomes
     * challenging, unless you make use of the dropdown type attribute.
     * </p>
     *
     *
     * @param type a custom picker dropdown type.
     * @return the component itself.
     */
    public TimePicker setType(String type) {
      pickerType = type;
      setUnrestrictedProperty("pickerType", type);
      return this;
    }

    /**
     * Gets the picker dropdown type.
     *
     * @return the picker dropdown type.
     * @see #setType(String)
     */
    public String getType() {
      return pickerType;
    }

    /**
     * Sets the step of the picker.
     *
     * @param step the step of the picker.
     * @return this component itself.
     */
    public TimePicker setStep(Duration step) {
      pickerStep = step;
      long millis = step.getSeconds() * 1000 + (long) (step.getNano() / 1_000_000.0);
      if (millis <= 0 || step.isNegative()) {
        throw new IllegalArgumentException("Step must be positive and greater than 0 milliseconds");
      }

      boolean isNotDivisibleByDayOrHour = (86400000L % millis != 0) && (3600000L % millis != 0);
      if (isNotDivisibleByDayOrHour) {
        throw new IllegalArgumentException(
            "The step " + step + " does not divide evenly into a day or an hour.");
      }

      setUnrestrictedProperty("step", millis / 1000);
      return this;
    }

    /**
     * Gets the step of the picker.
     *
     * @return the step of the picker.
     */
    public Duration getStep() {
      return pickerStep;
    }

    /**
     * Opens the picker.
     */
    public TimePicker open() {
      pickerOpenInvoked = true;
      BBjInputT field = inferTimeField();
      if (field != null) {
        field.showTimePicker();
      }

      return this;
    }

    /**
     * Alias for {@link #open()}.
     */
    public TimePicker show() {
      return open();
    }
  }
}
