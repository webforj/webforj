package org.dwcj.component.field;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.Focusable;
import org.dwcj.component.HasReadOnly;
import org.dwcj.component.SelectionInfo;
import org.dwcj.component.TabTraversable;
import org.dwcj.component.TextAlignable;
import org.dwcj.component.TextHighlightable;
import org.dwcj.component.events.EventDispatcher;
import org.dwcj.component.events.EventListener;
import org.dwcj.component.events.shared.events.FocusGainedEvent;
import org.dwcj.component.events.shared.sinks.FocusGainedEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.util.BBjFunctionalityHelper;

/** A Field to enter Text. */
public final class Field extends AbstractDwcComponent
    implements HasReadOnly, Focusable, TabTraversable, TextAlignable, TextHighlightable {

  private BBjEditBox bbjEditBox;


  private Integer maxLength = 2147483647;
  private FieldType type;
  private final EventDispatcher dispatcher = new EventDispatcher();
  private FocusGainedEventSink focusGainedEventSink;

  /** Enum to descripe the Fields types. */
  enum FieldType {
    /** A control for specifying a color; opening a color picker when active. */
    COLOR,

    /** A control for entering a date (year, month, and day, with no time). 
     * Opens a date picker or numeric wheels for year, month, day when active. 
     */
    DATE,
    
    /** A control for entering a date and time, with no time zone. 
     * Opens a date picker or numeric wheels for date- and time-components when active. 
     */
    DATETIME,
    
    /** A field for editing an email address. 
     * Looks like a text input, but has validation parameters. 
     */
    EMAIL,
    
    /** A control that lets the user select a file. 
     * Use the accept attribute to define the types of files that the control can select.
     */
    FILE,
    
    /** A control for entering a month and year, with no time zone. */
    MONTH,

    /** A control for entering a number. 
     * Displays a spinner and adds default validation. 
     */
    NUMBER,

    /** A single-line text field whose value is obscured. */
    PASSWORD,

    /** A control for entering a number whose exact value is not important. 
     * Displays as a range widget defaulting to the middle value. 
     * Used in conjunction min and max to define the range of acceptable values.
     */
    RANGE,

    /** A single-line text field for entering search strings. 
     * Line-breaks are automatically removed from the input value. 
     * May include a delete icon in supporting browsers that can be used to clear the field. 
     * Displays a search icon instead of enter key on some devices with dynamic keypads. 
     */
    SEARCH,

    /** A control for entering a telephone number. */
    TEL,

    /** The default value. 
     * A single-line text field. 
     * Line-breaks are automatically removed from the input value. 
     */
    TEXT,

    /** A control for entering a time value with no time zone. */
    TIME,

    /** A field for entering a URL. 
     * Looks like a text input, but has validation parameters.
     */
    URL,

    /** A control for entering a date consisting 
     * of a week-year number and a week number with no time zone. 
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

  /** Constructor which takes a initial text to display and the field type. */
  public Field(String text, FieldType type) {
    setText(text);
    setType(type);
    this.readOnly = false;
    this.focusable = true;
    this.tabTraversable = true;
    this.textAlignment = Alignment.LEFT;
    this.textHighlight = Highlight.HIGHLIGHT_NONE;

  }

  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      ctrl = w.addEditBox(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1,
          BASISNUMBER_1, getText(), flags);
      this.focusGainedEventSink = new FocusGainedEventSink(this, dispatcher);
      bbjEditBox = (BBjEditBox) this.ctrl;
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }

  }

  /**
   * Adds a click event for the Button component.
   *
   * @param listener The event
   * @return The component itself
   */
  public Field addFocusGained(EventListener<FocusGainedEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(FocusGainedEvent.class) == 0) {
      this.focusGainedEventSink.setCallback();
    }
    dispatcher.addEventListener(FocusGainedEvent.class, listener);
    return this;
  }


  /**
   * Removes a click event from the Button component.
   *
   * @param listener The event to be removed
   * @return The component itself
   */
  public Field removeFocusGained(EventListener<FocusGainedEvent> listener) {
    dispatcher.removeEventListener(FocusGainedEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(FocusGainedEvent.class) == 0) {
      this.focusGainedEventSink.removeCallback();
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

  /** Getter for the selcted text. */
  public String getSelectedText() {
    if (this.ctrl != null) {
      try {
        return bbjEditBox.getSelectedText();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return "";
  }

  /** Getter for the info on the current selection. */
  public SelectionInfo getSelectionInfo() {
    if (this.ctrl != null) {
      try {
        BBjVector vec = bbjEditBox.getSelection();
        Integer offsetLeft = (Integer) vec.get(1);
        Integer offsetRight = (Integer) vec.get(3);
        return new SelectionInfo(offsetLeft, offsetRight, this.getSelectedText());
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return null;
  }

  /** Selects a part of the text based on the provided offsets. */
  public Field select(Integer offsetLeft, Integer offsetRight) {
    if (this.ctrl != null) {
      bbjEditBox.select(offsetLeft, offsetRight);
    }
    return this;
  }

  /** Setter for the max amount of characters for this field. */
  public Field setMaxLength(Integer length) {
    if (this.ctrl != null) {
      try {
        bbjEditBox.setMaxLength(length);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.maxLength = length;
    return this;
  }

  /** Setter for the fields type. */
  public Field setType(FieldType type) {
    this.type = type;
    if (ctrl == null) {
      return this;
    }

    try {
      ctrl.setAttribute("type", type.toString());
      return this;
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to set type for the field.", e);
    }
  }

  /** Getter for the field type. */
  public FieldType getType() {
    return this.type;
  }

  @Override
  public Boolean isReadOnly() {
    if (this.ctrl != null) {
      try {
        return !bbjEditBox.isEditable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.readOnly;
  }

  @Override
  public Field setReadOnly(Boolean editable) {
    if (this.ctrl != null) {
      try {
        bbjEditBox.setEditable(!editable);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.readOnly = editable;
    return this;
  }

  @Override
  public Boolean isFocusable() {
    if (this.ctrl != null) {
      try {
        bbjEditBox.isFocusable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.focusable;
  }

  @Override
  public Field setFocusable(Boolean focusable) {
    if (this.ctrl != null) {
      try {
        bbjEditBox.setFocusable(focusable);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.focusable = focusable;
    return this;
  }

  @Override
  public Boolean isTabTraversable() {
    if (this.ctrl != null) {
      try {
        bbjEditBox.isTabTraversable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.tabTraversable;
  }

  @Override
  public Field setTabTraversable(Boolean traversable) {
    if (this.ctrl != null) {
      try {
        bbjEditBox.setTabTraversable(traversable);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.tabTraversable = traversable;
    return this;
  }

  @Override
  public Alignment getTextAlignment() {
    return this.textAlignment;
  }

  @Override
  public Field setTextAlignment(Alignment alignment) {
    if (this.ctrl != null) {
      try {
        bbjEditBox.setAlignment(alignment.textPosition);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.textAlignment = alignment;
    return this;
  }

  @Override
  public Highlight getHighlightOnFocus() {
    return this.textHighlight;
  }

  @Override
  public Field setHighlightOnFocus(Highlight highlight) {
    if (this.ctrl != null) {
      try {
        bbjEditBox.setHighlightOnFocus(highlight.highlightType);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.textHighlight = highlight;
    return this;
  }

  @Override
  public Field setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public Field setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public Field setEnabled(Boolean enabled) {
    super.setEnabled(enabled);
    return this;
  }

  @Override
  public Field setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  @Override
  public Field setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public Field setId(String elementId) {
    super.setId(elementId);
    return this;
  }

  @Override
  public Field setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  @Override
  public Field addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  @Override
  public Field removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }

  @Override
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp())) {
      throw new IllegalAccessException("catchUp cannot be called twice");
    }
    super.catchUp();


    if (this.dispatcher.getListenersCount(FocusGainedEvent.class) > 0) {
      this.focusGainedEventSink.setCallback();
    }

    if (this.maxLength != 2147483647) {
      this.setMaxLength(this.maxLength);
    }

    if (this.type != FieldType.TEXT) {
      this.setType(type);
    }

    if (Boolean.TRUE.equals(this.readOnly)) {
      this.setReadOnly(this.readOnly);
    }

    if (Boolean.FALSE.equals(this.focusable)) {
      this.setFocusable(this.focusable);
    }

    if (Boolean.FALSE.equals(this.tabTraversable)) {
      this.setTabTraversable(this.tabTraversable);
    }

    if (this.textAlignment != Alignment.LEFT) {
      this.setTextAlignment(this.textAlignment);
    }

    if (this.textHighlight != Highlight.HIGHLIGHT_NONE) {
      this.setHighlightOnFocus(this.textHighlight);
    }
  }
}
