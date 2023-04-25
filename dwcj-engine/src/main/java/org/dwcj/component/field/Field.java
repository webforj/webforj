package org.dwcj.component.field;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.Focusable;
import org.dwcj.component.HasPlaceholder;
import org.dwcj.component.HasReadOnly;
import org.dwcj.component.SelectionInfo;
import org.dwcj.component.TextAlignable;
import org.dwcj.component.TextHighlightable;
import org.dwcj.component.event.BlurEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.FocusEvent;
import org.dwcj.component.event.KeypressEvent;
import org.dwcj.component.event.ModifyEvent;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.MouseExitEvent;
import org.dwcj.component.event.RightMouseDownEvent;
import org.dwcj.component.event.sink.BlurEventSink;
import org.dwcj.component.event.sink.FieldKeypressEventSink;
import org.dwcj.component.event.sink.FocusEventSink;
import org.dwcj.component.event.sink.ModifyEventSink;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.dwcj.component.event.sink.RightMouseDownEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.util.BBjFunctionalityHelper;

/** A Field to enter Text. */
public final class Field extends AbstractDwcComponent
    implements HasReadOnly, Focusable, TextAlignable, TextHighlightable, HasPlaceholder {

  private BBjEditBox bbjEditBox;

  private Integer maxLength = 2147483647;
  private FieldType type;
  private String placeholder = "";
  private String name = "";
  private Boolean required = false;

  private final EventDispatcher dispatcher = new EventDispatcher();
  private FocusEventSink focusEventSink;
  private BlurEventSink blurEventSink;
  private MouseEnterEventSink mouseEnterEventSink;
  private MouseExitEventSink mouseExitEventSink;
  private RightMouseDownEventSink rightMouseDownEventSink;
  private ModifyEventSink modifyEventSink;
  private FieldKeypressEventSink fieldKeypressEventSink;

  /** Enum to describe the Fields types. */
  public enum FieldType {
    /** A value for specifying a color; opening a color picker when active. */
    COLOR,

    /**
     * A value for entering a date (year, month, and day, with no time). Opens a date picker or
     * numeric wheels for year, month, day when active.
     */
    DATE,

    /**
     * A value for entering a date and time, with no time zone. Opens a date picker or numeric
     * wheels for date- and time-components when active.
     */
    DATETIME,

    /**
     * A field for editing an email address. Looks like a text input, but has validation parameters.
     */
    EMAIL,

    /**
     * A value that lets the user select a file. Use the accept attribute to define the types of
     * files that the component can select.
     */
    FILE,

    /** A value for entering a month and year, with no time zone. */
    MONTH,

    /**
     * A value for entering a number. Displays a spinner and adds default validation.
     */
    NUMBER,

    /** A single-line text field whose value is obscured. */
    PASSWORD,

    /**
     * A value for entering a number whose exact value is not important. Displays as a range widget
     * defaulting to the middle value. Used in conjunction min and max to define the range of
     * acceptable values.
     */
    RANGE,

    /**
     * A single-line text field for entering search strings. Line-breaks are automatically removed
     * from the input value. May include a delete icon in supporting browsers that can be used to
     * clear the field. Displays a search icon instead of enter key on some devices with dynamic
     * keypads.
     */
    SEARCH,

    /** A value for entering a telephone number. */
    TEL,

    /**
     * The default value. A single-line text field. Line-breaks are automatically removed from the
     * input value.
     */
    TEXT,

    /** A value for entering a time value with no time zone. */
    TIME,

    /**
     * A field for entering a URL. Looks like a text input, but has validation parameters.
     */
    URL,

    /**
     * A value for entering a date consisting of a week-year number and a week number with no time
     * zone.
     */
    WEEK;

    @Override
    public String toString() {
      if (this == DATETIME) {
        return "datetime-local";
      }
      return super.toString().toLowerCase();
    }
  }

  public Field() {
    this("", FieldType.TEXT);
  }

  public Field(String text) {
    this(text, FieldType.TEXT);
  }

  public Field(FieldType type) {
    this("", type);
  }

  public static Field password() {
    return new Field(FieldType.PASSWORD);
  }

  /** Constructor which takes a initial text to display and the field type. */
  public Field(String text, FieldType type) {
    setText(text);
    setType(type);
    this.readOnly = false;
    this.focusable = true;
    this.textAlignment = Alignment.LEFT;
    this.textHighlight = Highlight.HIGHLIGHT_NONE;
  }

  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      ctrl = w.addEditBox(getText(), flags);
      this.focusEventSink = new FocusEventSink(this, dispatcher);
      this.blurEventSink = new BlurEventSink(this, dispatcher);
      this.mouseEnterEventSink = new MouseEnterEventSink(this, dispatcher);
      this.mouseExitEventSink = new MouseExitEventSink(this, dispatcher);
      this.rightMouseDownEventSink = new RightMouseDownEventSink(this, dispatcher);
      this.modifyEventSink = new ModifyEventSink(this, dispatcher);
      this.fieldKeypressEventSink = new FieldKeypressEventSink(this, dispatcher);

      bbjEditBox = (BBjEditBox) this.ctrl;
      catchUp();
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to create Field.", e);
    }
  }


  /**
   * Adds a keypress event for the Field component.
   *
   * @param listener The event
   * @return The component itself
   */
  public Field addKeypressListener(EventListener<KeypressEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(KeypressEvent.class) == 0) {
      this.fieldKeypressEventSink.setCallback();
    }
    dispatcher.addEventListener(KeypressEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addKeypressListener method.
   *
   * @see Field #addKeypressListener(EventListener)
   * @param listener A method to receive the keypress event
   * @return the component itself
   */
  public Field onKeypress(EventListener<KeypressEvent> listener) {
    return addKeypressListener(listener);
  }

  /**
   * Removes a keypress event from the Field component.
   *
   * @param listener The event to be removed
   * @return The component itself
   */
  public Field removeKeypressListener(EventListener<KeypressEvent> listener) {
    dispatcher.removeEventListener(KeypressEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(KeypressEvent.class) == 0) {
      this.fieldKeypressEventSink.removeCallback();
    }
    return this;
  }


  /**
   * Adds a modify event for the Field component.
   *
   * @param listener The event
   * @return The component itself
   */
  public Field addModifyListener(EventListener<ModifyEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(ModifyEvent.class) == 0) {
      this.modifyEventSink.setCallback();
    }
    dispatcher.addEventListener(ModifyEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addModifyListener method.
   *
   * @see Field #addModifyListener(EventListener)
   * @param listener A method to receive the modify event
   * @return the component itself
   */
  public Field onModify(EventListener<ModifyEvent> listener) {
    return addModifyListener(listener);
  }

  /**
   * Removes a modify event from the Field component.
   *
   * @param listener The event to be removed
   * @return The component itself
   */
  public Field removeModifyListener(EventListener<ModifyEvent> listener) {
    dispatcher.removeEventListener(ModifyEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(ModifyEvent.class) == 0) {
      this.modifyEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a focus event for the Field component.
   *
   * @param listener The event
   * @return The component itself
   */
  public Field addFocusListener(EventListener<FocusEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(FocusEvent.class) == 0) {
      this.focusEventSink.setCallback();
    }
    dispatcher.addEventListener(FocusEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addFocusListener method.
   *
   * @see Field #addFocusListener(EventListener)
   * @param listener A method to receive the focus event
   * @return the component itself
   */
  public Field onFocus(EventListener<FocusEvent> listener) {
    return addFocusListener(listener);
  }

  /**
   * Removes a focus event from the Field component.
   *
   * @param listener The event to be removed
   * @return The component itself
   */
  public Field removeFocusListener(EventListener<FocusEvent> listener) {
    dispatcher.removeEventListener(FocusEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(FocusEvent.class) == 0) {
      this.focusEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a blur event for the Field component. A blur event fires when a component looses focus.
   *
   * @param listener The event
   * @return The component itself
   */
  public Field addBlurListener(EventListener<BlurEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(BlurEvent.class) == 0) {
      this.blurEventSink.setCallback();
    }
    dispatcher.addEventListener(BlurEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addBlurListener method.
   *
   * @see Field #addBlurListener(EventListener)
   * @param listener A method to receive the blur event
   * @return the component itself
   */
  public Field onBlur(EventListener<BlurEvent> listener) {
    return addBlurListener(listener);
  }

  /**
   * Removes a blur event from the Field component. Fires when a component looses focus.
   *
   * @param listener The event to be removed
   * @return The component itself
   */
  public Field removeBlurListener(EventListener<BlurEvent> listener) {
    dispatcher.removeEventListener(BlurEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(BlurEvent.class) == 0) {
      this.blurEventSink.removeCallback();
    }
    return this;
  }


  /**
   * Adds a mouse enter event for the Field component.
   *
   * @param listener The event
   * @return The component itself
   */
  public Field addMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseEnterEvent.class) == 0) {
      this.mouseEnterEventSink.setCallback();
    }
    dispatcher.addEventListener(MouseEnterEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addMouseEnterListener method.
   *
   * @see Field #addMouseEnterListener(EventListener)
   * @param listener A method to receive the mouse enter event
   * @return the component itself
   */
  public Field onMouseEnter(EventListener<MouseEnterEvent> listener) {
    return addMouseEnterListener(listener);
  }

  /**
   * Removes a mouse enter event from the Field component.
   *
   * @param listener The event to be removed
   * @return The component itself
   */
  public Field removeMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    dispatcher.removeEventListener(MouseEnterEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseEnterEvent.class) == 0) {
      this.mouseEnterEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a mouse exit event for the Field component.
   *
   * @param listener The event
   * @return The component itself
   */
  public Field addMouseExitListener(EventListener<MouseExitEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseExitEvent.class) == 0) {
      this.mouseExitEventSink.setCallback();
    }
    dispatcher.addEventListener(MouseExitEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addMouseEnterListener method.
   *
   * @see Field #addMouseEnterListener(EventListener)
   * @param listener A method to receive the mouse enter event
   * @return the component itself
   */
  public Field onMouseExit(EventListener<MouseExitEvent> listener) {
    return addMouseExitListener(listener);
  }

  /**
   * Removes a mouse enter event from the Field component.
   *
   * @param listener The event to be removed
   * @return The component itself
   */
  public Field removeMouseExitListener(EventListener<MouseExitEvent> listener) {
    dispatcher.removeEventListener(MouseExitEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseExitEvent.class) == 0) {
      this.mouseExitEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a right mouse down event for the Field component.
   *
   * @param listener The event
   * @return The component itself
   */
  public Field addRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(RightMouseDownEvent.class) == 0) {
      this.rightMouseDownEventSink.setCallback();
    }
    dispatcher.addEventListener(RightMouseDownEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addRightMouseDownListener method.
   *
   * @see Field #addRightMouseDownListener(EventListener)
   * @param listener A method to receive the right mouse down event
   * @return the component itself
   */
  public Field onRightMouseDown(EventListener<RightMouseDownEvent> listener) {
    return addRightMouseDownListener(listener);
  }

  /**
   * Removes a right mouse down event from the Field component.
   *
   * @param listener The event to be removed
   * @return The component itself
   */
  public Field removeRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    dispatcher.removeEventListener(RightMouseDownEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(RightMouseDownEvent.class) == 0) {
      this.rightMouseDownEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Getter for the max length of this field.
   *
   * @return the max amount of character this field is allowed to hold
   */
  public Integer getMaxLength() {
    if (this.ctrl != null) {
      return bbjEditBox.getMaxLength();
    }
    return this.maxLength;
  }

  /**
   * Getter for the selected text.
   *
   * @return A string value of the text selected by the user.
   */
  public String getSelectedText() {
    if (this.ctrl != null) {
      try {
        return bbjEditBox.getSelectedText();
      } catch (BBjException e) {
        throw new DwcjRuntimeException("Failed to get selected text.", e);
      }
    }
    return "";
  }

  /**
   * Getter for the info on the current selection.
   *
   * @see SelectionInfo
   * @return A SelectionInfo object.
   */
  public SelectionInfo getSelectionInfo() {
    if (this.ctrl != null) {
      try {
        BBjVector vec = bbjEditBox.getSelection();
        Integer offsetLeft = (Integer) vec.get(1);
        Integer offsetRight = (Integer) vec.get(3);
        if (offsetLeft == null) {
          offsetLeft = 0;
        }
        if (offsetRight == null) {
          offsetRight = 0;
        }
        return new SelectionInfo(offsetLeft, offsetRight, this.getSelectedText());
      } catch (BBjException e) {
        throw new DwcjRuntimeException("Failed to get selection info.", e);
      }
    }
    return new SelectionInfo(0, 0, "");
  }

  /**
   * Selects a part of the text based on the provided offsets.
   *
   * @return The object itself.
   */
  public Field select(Integer offsetLeft, Integer offsetRight) {
    if (this.ctrl != null) {
      try {
        bbjEditBox.focus();
      } catch (Exception e) {
        throw new DwcjRuntimeException("Failed to select the text.", e);
      }
      bbjEditBox.select(offsetLeft, offsetRight);
    }
    return this;
  }

  /**
   * Setter for the max amount of characters for this field.
   *
   * @return The object itself.
   */
  public Field setMaxLength(Integer length) {
    if (this.ctrl != null) {
      try {
        bbjEditBox.setMaxLength(length);
      } catch (BBjException e) {
        throw new DwcjRuntimeException("Failed to set max length.", e);
      }
    }
    this.maxLength = length;
    return this;
  }

  /**
   * Setter for the fields type.
   *
   * @return The object itself.
   */
  public Field setType(FieldType type) {
    if (this.type == type) {
      return this;
    }

    this.type = type;
    if (ctrl == null) {
      return this;
    }

    try {
      super.setAttribute("type", type.toString());
      return this;
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to set type for the field.", e);
    }
  }

  /**
   * Getter for the field type.
   *
   * @see FieldType
   * @return The applicable FieldType enum value
   */
  public FieldType getType() {
    return this.type;
  }

  /**
   * Sets the hint for expected file type in file upload components.
   *
   * @param types expected file types
   * @return The component itself
   */
  public Field setExpectedFileTypes(String types) {
    super.setProperty("accept", types);
    return this;
  }

  /**
   * Returns the file types in file upload components.
   *
   * @return The expected file types
   */
  public String getExpectedFileTypes() {
    return (String) super.getProperty("accept");
  }

  /**
   * Enables autocorrection. Safari only.
   *
   * @param enabled true if enabled, false otherwise
   * @return The component itself
   */
  public Field setAutocorrect(Boolean enabled) {
    final String value;
    if (Boolean.TRUE.equals(enabled)) {
      value = "on";
    } else {
      value = "off";
    }
    super.setProperty("autocorrect", value);
    return this;
  }

  /**
   * Returns whether autocorrection is enbaled or not.
   *
   * @return True if enabled, false otherwise
   */
  public Boolean isAutocorrect() {
    final String value = (String) super.getProperty("autocorrect");
    return value.equals("on");
  }

  /**
   * Automatically focus the component when the page is loaded.
   *
   * @param enabled true for enabling, false otherwise
   * @return The component itself
   */
  public Field setAutofocus(Boolean enabled) {
    super.setProperty("autofocus", enabled);
    return this;
  }

  /**
   * Returns if the component gets autofocused when the page is loaded.
   *
   * @return True if enabled, false otherwise
   */
  public Boolean isAutofocus() {
    return (Boolean) super.getProperty("autofocus");
  }

  /**
   * Returns if the component is currently focused.
   *
   * @return True if the component currently has focus, false otherwise
   */
  public Boolean hasFocus() {
    return Boolean.parseBoolean(super.getAttribute("has-focus"));
  }

  /**
   * The Fields label.
   *
   * @param label the label displayed
   * @return The component itself
   */
  public Field setLabel(String label) {
    super.setProperty("label", label);
    return this;
  }

  /**
   * Returns the Fields label.
   *
   * @return Component's label
   */
  public String getLabel() {
    return (String) super.getProperty("label");
  }

  /**
   * Sets the maximum for numeric types.
   *
   * @return The maximum set for the component
   */
  public Field setMax(Integer max) {
    super.setProperty("max", max);
    return this;
  }

  /**
   * Returns the maximum for numeric types.
   *
   * @return The maximum value of the component
   */
  public Integer getMax() {
    return (Integer) super.getProperty("max");
  }

  /**
   * Sets the minimum for numeric types.
   *
   * @return The component itself
   */
  public Field setMin(Integer min) {
    super.setProperty("min", min);
    return this;
  }

  /**
   * Returns the minimum for numeric types.
   *
   * @return The minimum value for the component
   */
  public Integer getMin() {
    return (Integer) super.getProperty("min");
  }

  /**
   * Sets the minimum amount of characters.
   *
   * @param min the amount of required characters
   * @return The component itself
   */
  public Field setMinLength(Integer min) {
    super.setProperty("minLength", min);
    return this;
  }

  /**
   * Returns the minimum amount of characters.
   *
   * @return The minimum amount of characters
   */
  public Integer getMinLength() {
    return (Integer) super.getProperty("minLength");
  }

  /**
   * Sets the name of the component.
   *
   * @param name the name
   * @return The component itself
   */
  public Field setName(String name) {
    if (this.name.equals(name)) {
      return this;
    }

    this.name = name;
    if (ctrl == null) {
      return this;
    }

    try {
      super.setAttribute("name", name);
      return this;
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to set name for the field.", e);
    }
  }

  /**
   * Returns the name of this component.
   *
   * @return The name of the component
   */
  public String getName() {
    return this.name;
  }

  /**
   * Show/Hide the password if the Field is of type password.
   *
   * @param show true to show the password, false to hide it
   * @return The component itself
   */
  public Field showPassword(Boolean show) {
    super.setProperty("password-reveal", show);
    return this;
  }

  /**
   * Sets whether or not the component should force a value to be entered.
   *
   * @param required true to force a value, false if it can be empty
   * @return The component itself
   */
  public Field setRequired(Boolean required) {
    if (this.required.equals(required)) {
      return this;
    }

    this.required = required;

    if (ctrl == null) {
      return this;
    }

    try {
      super.setAttribute("required", required.toString());
      return this;
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to set required for the field.", e);
    }
  }

  /**
   * Returns whether or not this component requires a value.
   *
   * @return True if the field requires value, false otherwise
   */
  public Boolean isRequired() {
    return this.required;
  }


  /**
   * Set the size of the component.
   *
   * @param size the size
   * @return the component itself
   */
  public Field setSize(Integer size) {
    super.setProperty("size", size);
    return this;
  }

  /**
   * Returns the size of the component.
   *
   * @return the size
   */
  public Integer getSize() {
    return (Integer) super.getProperty("size");
  }


  /**
   * Enables spellcheck.
   *
   * @param enabled true if spellcheck is enabled, false otherwise
   * @return The component itself
   */
  public Field setSpellcheck(Boolean enabled) {
    super.setProperty("spellcheck", enabled);
    return this;
  }

  /**
   * Returns the spellcheck value for this component.
   *
   * @return True if spellcheck is enabled, false otherwise
   */
  public Boolean isSpellcheck() {
    return (Boolean) super.getProperty("spellcheck");
  }


  /**
   * Sets the incremental step for when the component type is range.
   *
   * @param step the amount of each step
   * @return The component itself
   */
  public Field setStep(Integer step) {
    super.setProperty("step", step);
    return this;
  }

  /**
   * Returns the step value for this component.
   *
   * @return The amount of each step
   */
  public Integer getStep() {
    return (Integer) super.getProperty("step");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Boolean isReadOnly() {
    if (this.ctrl != null) {
      try {
        return !bbjEditBox.isEditable();
      } catch (BBjException e) {
        throw new DwcjRuntimeException("Failed to read read only setting.", e);
      }
    }
    return this.readOnly;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Field setReadOnly(Boolean editable) {
    if (this.ctrl != null) {
      try {
        bbjEditBox.setEditable(!editable);
      } catch (BBjException e) {
        throw new DwcjRuntimeException("Failed to set read only.", e);
      }
    }
    this.readOnly = editable;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Boolean isFocusable() {
    if (this.ctrl != null) {
      try {
        bbjEditBox.isFocusable();
      } catch (BBjException e) {
        throw new DwcjRuntimeException("Failed to read focusable setting.", e);
      }
    }
    return this.focusable;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Field setFocusable(Boolean focusable) {
    if (this.ctrl != null) {
      try {
        bbjEditBox.setFocusable(focusable);
      } catch (BBjException e) {
        throw new DwcjRuntimeException("Failed to set focusable.", e);
      }
    }
    this.focusable = focusable;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Alignment getTextAlignment() {
    return this.textAlignment;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Field setTextAlignment(Alignment alignment) {
    if (this.ctrl != null) {
      try {
        bbjEditBox.setAlignment(alignment.textPosition);
      } catch (BBjException e) {
        throw new DwcjRuntimeException("Failed to set alignment.", e);
      }
    }
    this.textAlignment = alignment;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Highlight getHighlightOnFocus() {
    return this.textHighlight;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Field setHighlightOnFocus(Highlight highlight) {
    if (this.ctrl != null) {
      try {
        bbjEditBox.setHighlightOnFocus(highlight.highlightType);
      } catch (BBjException e) {
        throw new DwcjRuntimeException("Failed to set highlight on focus.", e);
      }
    }
    this.textHighlight = highlight;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Field setText(String text) {
    super.setText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Field setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Field setEnabled(Boolean enabled) {
    super.setEnabled(enabled);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Field setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Field setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Field removeAttribute(String attribute) {
    super.removeAttribute(attribute);
    return this;
  }


  /**
   * {@inheritDoc}
   */
  @Override
  public Field setUserData(String key, Object userData) {
    super.setUserData(key, userData);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Field setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Field removeStyle(String property) {
    super.removeStyle(property);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Field addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Field removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Field setProperty(String property, Object value) {
    super.setProperty(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getPlaceholder() {
    return this.placeholder;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Field setPlaceholder(String placeholder) {
    this.placeholder = placeholder;
    if (this.ctrl == null) {
      return this;
    }

    try {
      this.bbjEditBox.setPlaceholder(placeholder);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to set the placeholder.", e);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp())) {
      throw new IllegalAccessException("catchUp cannot be called twice");
    }
    super.catchUp();


    if (this.dispatcher.getListenersCount(FocusEvent.class) > 0) {
      this.focusEventSink.setCallback();
    }

    if (this.dispatcher.getListenersCount(BlurEvent.class) > 0) {
      this.blurEventSink.setCallback();
    }

    if (this.dispatcher.getListenersCount(MouseEnterEvent.class) > 0) {
      this.mouseEnterEventSink.setCallback();
    }

    if (this.dispatcher.getListenersCount(MouseExitEvent.class) > 0) {
      this.mouseExitEventSink.setCallback();
    }

    if (this.dispatcher.getListenersCount(RightMouseDownEvent.class) > 0) {
      this.rightMouseDownEventSink.setCallback();
    }

    if (this.dispatcher.getListenersCount(ModifyEvent.class) > 0) {
      this.modifyEventSink.setCallback();
    }

    if (this.dispatcher.getListenersCount(KeypressEvent.class) > 0) {
      this.fieldKeypressEventSink.setCallback();
    }

    if (this.maxLength != 2147483647) {
      this.setMaxLength(this.maxLength);
    }

    if (this.placeholder != null) {
      this.setPlaceholder(placeholder);
    }

    if (Boolean.TRUE.equals(this.readOnly)) {
      this.setReadOnly(this.readOnly);
    }

    if (Boolean.TRUE.equals(this.required)) {
      this.setRequired(required);
    }

    if (this.name != null) {
      this.setName(name);
    }

    if (Boolean.FALSE.equals(this.focusable)) {
      this.setFocusable(this.focusable);
    }

    if (this.textAlignment != Alignment.LEFT) {
      this.setTextAlignment(this.textAlignment);
    }

    if (this.textHighlight != Highlight.HIGHLIGHT_NONE) {
      this.setHighlightOnFocus(this.textHighlight);
    }
  }
}