package com.webforj.component.maskednumberfield;

import com.basis.bbj.proxies.sysgui.BBjInputN;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.function.Consumer;
import javax.swing.text.Highlighter.Highlight;
import com.webforj.Environment;
import com.webforj.annotation.ExcludeFromJacocoGeneratedReport;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.LegacyDwcComponent;
import com.webforj.component.maskednumberfield.event.MaskedNumberFieldModifyEvent;
import com.webforj.component.maskednumberfield.sink.MaskedNumberFieldModifyEventSink;
import com.webforj.component.text.Label;
import com.webforj.component.window.Window;
import com.webforj.concern.HasHighlightOnFocus;
import com.webforj.concern.HasHorizontalAlignment;
import com.webforj.concern.legacy.LegacyHasEnable;
import com.webforj.concern.legacy.LegacyHasFocus;
import com.webforj.concern.legacy.LegacyHasReadOnly;
import com.webforj.concern.legacy.LegacyHasTabTraversal;
import com.webforj.utilities.BBjFunctionalityHelper;


public class MaskedNumberField extends LegacyDwcComponent
    implements LegacyHasReadOnly, LegacyHasFocus, LegacyHasTabTraversal, LegacyHasEnable,
    HasHorizontalAlignment<MaskedNumberField>, HasHighlightOnFocus<MaskedNumberField> {

  protected BBjInputN numBox;

  public enum Expanse {
    LARGE, MEDIUM, SMALL, XLARGE, XSMALL
  }

  public enum Theme {
    DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
  }


  protected ArrayList<Consumer<MaskedNumberFieldModifyEvent>> callbacks = new ArrayList<>();
  protected MaskedNumberFieldModifyEventSink editModifyEventSink;


  protected String commaChar = ",";
  protected String dotChar = ".";
  protected String editString = "";
  protected Boolean highlight = false;
  protected Boolean insertMode = true;
  protected Integer length = 9;
  protected Integer margin = 3;
  protected String mask = "-########";
  protected Boolean negatable = true;
  protected Boolean pEnter = false;
  protected Boolean pTab = false;
  protected float restore = 0;
  protected Boolean commas = false;
  protected BigDecimal value = BigDecimal.valueOf(0);



  public MaskedNumberField(String text) {
    setText(text);
    this.readOnly = false;
    this.tabTraversable = true;
    setComponentHorizontalAlignment(Alignment.RIGHT);
  }

  public MaskedNumberField() {
    this("");
  }

  @Override
  protected void onCreate(Window p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      control = w.addInputN(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1,
          BASISNUMBER_1, flags);
      numBox = (BBjInputN) this.control;
      onAttach();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public MaskedNumberField onEditModify(Consumer<MaskedNumberFieldModifyEvent> callback) {
    if (this.control != null) {
      if (this.editModifyEventSink == null) {
        this.editModifyEventSink = new MaskedNumberFieldModifyEventSink(this);
      }
      this.editModifyEventSink.addCallback(callback);
    } else {
      this.callbacks.add(callback);
    }
    return this;
  }

  /**
   * Returns the comma character used in the NumericBox control.
   *
   * @return Returns the character that is used as the comma in the control.
   */
  public String getCommaCharacter() {
    if (this.control != null) {
      try {
        return numBox.getCommaCharacter();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.commaChar;
  }

  /**
   * Returns the character to be used indicating the decimal point in the NumericBox control.
   *
   * @return Returns the character to be used indicating the decimal point.
   */
  public String getDotCharacter() {
    if (this.control != null) {
      try {
        return numBox.getDotCharacter();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.dotChar;
  }

  /**
   * Returns the EDIT string which is used to map input characters to edit functions for the
   * NumericBox control.
   *
   * @return Returns the EDIT string which is used to map input characters to edit functions for the
   *         control.
   */
  public String getEditString() {
    if (this.control != null) {
      try {
        return numBox.getEditString().toString();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.editString;
  }

  /**
   * Returns the number of the last error generated in the NumericBox control.
   *
   * @return Returns the number of the last error generated in the control.
   */
  public Integer getError() {
    if (this.control != null) {
      try {
        return numBox.getError();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return null;
  }

  /**
   * Returns whether the text in the NumericBox control is highlighted when focus is gained from
   * tabbing into the control.
   *
   * @return Returns whether the text in the control is highlighted (false = Not highlighted, true =
   *         highlighted).
   */
  public Boolean isHighlight() {
    if (this.control != null) {
      try {
        return numBox.getHighlight();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.highlight;
  }

  /**
   * Returns whether the NumericBox control is in insert mode.
   *
   * @return Returns whether the control is in the insert mode (false = Not in insert mode, true =
   *         In insert mode).
   */
  public Boolean isInsertMode() {
    if (this.control != null) {
      try {
        return numBox.getInsertMode();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.insertMode;
  }

  /**
   * Returns the length of the text in the NumericBox control.
   *
   * @return Returns the length of the text in the control.
   */
  public int getLength() {
    if (this.control != null) {
      try {
        return numBox.getLength();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.length;
  }

  /**
   * This method returns the left margin of the NumericBox control.
   *
   * @return Returns the left margin of the control.
   */
  public Integer getMargin() {
    if (this.control != null) {
      try {
        return numBox.getMargin();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.margin;
  }

  /**
   * Returns the mask of the NumericBox control.
   *
   * @return Returns the mask of the control, which provides character-type verification.
   */
  public String getMask() {
    if (this.control != null) {
      try {
        return numBox.getMask();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.mask;
  }

  /**
   * Returns whether the NumericBox control will accept negative values.
   *
   * @return Returns whether this control will accept negative values.
   */
  public Boolean isNegatable() {
    if (this.control != null) {
      try {
        return numBox.getNegateable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.negatable;
  }

  /**
   * Returns whether the NumericBox control passes the ENTER key to the top-level window.
   *
   * @return Returns whether the ENTER key is passed to the top-level window (false = Not passed,
   *         true = Passed).
   */
  public Boolean isPassEnter() {
    if (this.control != null) {
      try {
        return numBox.getPassEnter();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.pEnter;
  }

  /**
   * Returns whether the NumericBox control passes the TAB key notification to the top-level window.
   *
   * @return Returns whether the TAB key notification is passed to the top-level window (false = Not
   *         passed, true = Passed) .
   */
  public Boolean isPassTab() {
    if (this.control != null) {
      try {
        return numBox.getPassTab();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.pTab;
  }

  /**
   * Returns the restore string of the NumericBox control.
   *
   * @return Returns the restore string of the BBjInputN control - the restore string is the text
   *         that appears when a user hits the restore key (usually ESC) or by calling the
   *         NumericBox::restore method.
   */
  public String getRestore() {
    if (this.control != null) {
      try {
        return numBox.getRestore();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return String.valueOf(restore);
  }

  /**
   * Returns whether comma characters are displayed while in edit mode in the NumericBox control.
   *
   * @return Returns whether comma characters are displayed while in edit mode (0 = Do not display
   *         commas, 1 = Display commas). By default, commas are not displayed.
   */
  public Boolean isUseEditCommas() {
    if (this.control != null) {
      try {
        return numBox.getUseEditCommas();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.commas;
  }

  /**
   * Returns the value of the contents of the NumericBox control.
   *
   * @return Returns the value of the contents of the control.
   */
  public BigDecimal getValue() {
    if (this.control != null) {
      try {
        return numBox.getValue().toBigDecimal();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.value;
  }



  /**
   * Sets the text in the NumericBox control to the restore string. The restore string is set in the
   * creation of the BBjInputN control or by calling the NumericBox::setRestoreString method.
   */
  public void restore() {
    if (this.control != null) {
      try {
        numBox.restore();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
  }

  /**
   * This method selects all of the NumericBox control's current contents as if the user highlighted
   * it with the mouse or keyboard.
   *
   * @return Returns this
   */
  public MaskedNumberField selectAll() {
    if (this.control != null) {
      try {
        numBox.selectAll();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  /**
   * Sets the comma character to be used in the NumericBox control. This will replace the character
   * used to indicate a comma mask character.
   *
   * @param comma - Specifies the character to use for a comma.
   * @return Returns this
   */
  public MaskedNumberField setCommaCharacter(String comma) {
    if (this.control != null) {
      try {
        numBox.setCommaCharacter(comma);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.commaChar = comma;
    return this;
  }

  /**
   * Sets the character used to indicate the decimal point. This will replace the character used to
   * indicate the decimal point.
   *
   * @param dot - Specifies the character to use, which indicates the decimal point.
   * @return Returns this
   */
  public MaskedNumberField setDotCharacter(String dot) {
    if (this.control != null) {
      try {
        numBox.setDotCharacter(dot);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.dotChar = dot;
    return this;
  }



  /**
   * Sets the EDIT string, which is used to map input characters to edit functions for theNumericBox
   * control.
   *
   * @param edit - Specifies the EDIT string.
   * @return Returns this
   */
  public MaskedNumberField setEditString(String edit) {
    if (this.control != null) {
      try {
        numBox.setEditString(edit.getBytes());
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.editString = edit;
    return this;
  }

  /**
   * Sets whether to highlight the text in the control when focus is gained from tabbing into the
   * control.
   *
   * @param highlight - Specifies whether the text is to be highlighted (false = No highlight, true
   *        = Highlight)
   * @return Returns this
   */
  public MaskedNumberField setHighlight(Boolean highlight) {
    if (this.control != null) {
      try {
        numBox.setHighlight(highlight);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.highlight = highlight;
    return this;
  }

  /**
   * Sets whether the NumericBox control is in insert mode.
   *
   * @param insert - Sets whether the control is in insert mode (false = Not in insert mode, true =
   *        In insert mode)
   * @return Returns this
   */
  public MaskedNumberField setInsertMode(Boolean insert) {
    if (this.control != null) {
      try {
        numBox.setInsertMode(insert);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.insertMode = insert;
    return this;
  }

  /**
   * Sets the length of the text in the NumericBox control. If the setLength method is called, then
   * the previous mask is not used. The mask for the characters will be the default mask character
   * "X".
   *
   * @param len - Specifies the length of the text in the control.
   * @return Returns this
   */
  public MaskedNumberField setLength(Integer len) {
    if (this.control != null) {
      try {
        numBox.setLength(len);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.length = len;
    return this;
  }

  /**
   * Sets the left margin of the NumericBox control.
   *
   * @param marginWidth - Width of the left margin.
   * @return Returns this
   */
  public MaskedNumberField setMargin(Integer marginWidth) {
    if (this.control != null) {
      try {
        numBox.setMargin(marginWidth);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.margin = marginWidth;
    return this;
  }

  /**
   * Sets the mask for the NumericBox control.
   *
   * @param mask - Specifies the mask that provides character-type verification.
   * @return Returns this
   */
  public MaskedNumberField setMask(String mask) {
    if (this.control != null) {
      try {
        numBox.setMask(mask);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.mask = mask;
    return this;
  }

  /**
   * Sets whether the NumericBox control accepts negative values. Negative values are initially
   * accepted.
   *
   * @param negatable - Specifies whether the BBjInputN will accept negative values (false =
   *        Negative values are not accepted, true = Negative values are accepted).
   * @return Returns this
   */
  public MaskedNumberField setNegatable(boolean negatable) {
    if (this.control != null) {
      try {
        numBox.setNegateable(negatable);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.negatable = negatable;
    return this;
  }

  /**
   * Sets whether the NumericBox control is to pass the ENTER key notification to the top-level
   * window. By default, the ENTER key is not passed.
   *
   * @param pass - Specifies whether to pass the ENTER key to the top-level window (false = Not
   *        passed, true = Passed).
   * @return Returns this
   */
  public MaskedNumberField setPassEnter(Boolean pass) {
    if (this.control != null) {
      try {
        numBox.setPassEnter(pass);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.pEnter = pass;
    return this;
  }

  /**
   * Sets whether the NumericBox control is to pass the TAB key notification to the top-level
   * window.
   *
   * @param pass - Specifies whether to pass the TAB key notification to the top-level window (false
   *        = Not passed, true = Passed).
   * @return Returns this
   */
  public MaskedNumberField setPassTab(Boolean pass) {
    if (this.control != null) {
      try {
        numBox.setPassEnter(pass);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.pTab = pass;
    return this;
  }

  /**
   * Sets the restore value of the NumericBox control. The restore value is the number that appears
   * when the user hits the restore key (usually ESC) or by calling the NumericBox::restore method.
   *
   * @param restore - Specifies the restore value.
   * @return Returns this
   */
  public MaskedNumberField setRestore(float restore) {
    if (this.control != null) {
      try {
        numBox.setRestore(String.valueOf(restore));
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.restore = restore;
    return this;
  }

  /**
   * Sets whether to display the comma characters in the mask while in edit mode in the NumericBox
   * control.
   *
   * @param useCommas - Specifies whether to display the comma characters in the mask while in edit
   *        mode (false = No display, true = Display)
   * @return Returns this
   */
  public MaskedNumberField setUseEditCommas(boolean useCommas) {
    if (this.control != null) {
      try {
        numBox.setUseEditCommas(useCommas);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.commas = useCommas;
    return this;
  }

  /**
   * Sets the value of the NumericBox control. Note that a mask within a control does NOT round, but
   * a mask within the STR() DOES round. For example, when placing a value such as 12.34567 into an
   * inputN control that is masked with ###0.00, you'll get 12.34. However, if in console mode you
   * print str(12.34567:"###0.00"), you'll get 12.35.
   *
   * @param value - Specifies the value to be set in the control.
   * @return Returns this
   */
  public MaskedNumberField setValue(BigDecimal value) {
    if (this.control != null) {
      try {
        numBox.setValue(BasisNumber.createBasisNumber(value));
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.value = value;
    return this;
  }

  public MaskedNumberField setValue(float value) {
    setValue(BigDecimal.valueOf(value));
    return this;
  }

  public MaskedNumberField setValue(int value) {
    setValue(BigDecimal.valueOf(value));
    return this;
  }

  /**
   * Returns whether the text in the NumericBox control can be edited.
   *
   * @return Returns whether the text in the control can be edited (false = Not Editable, true =
   *         Editable). By default, the text is editable.
   */
  @Override
  public Boolean isReadOnly() {
    if (this.control != null) {
      try {
        return !numBox.isEditable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.readOnly;
  }

  /**
   * Sets whether the text in the NumericBox control can be edited. By default, the control is
   * editable.
   *
   * @param editable - Sets the editability of the control (false = Not Editable, true = Editable)
   * @return Returns this
   */
  @Override
  public MaskedNumberField setReadOnly(Boolean editable) {
    if (this.control != null) {
      try {
        numBox.setEditable(!editable);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  @Override
  public MaskedNumberField focus() {
    super.focusComponent();
    return this;
  }

  @Override
  public Boolean isTabTraversable() {
    if (this.control != null) {
      try {
        numBox.isTabTraversable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.tabTraversable;
  }

  @Override
  public MaskedNumberField setTabTraversable(Boolean traverse) {
    if (this.control != null) {
      try {
        numBox.setTabTraversable(traverse);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.tabTraversable = traverse;
    return this;
  }

  @Override
  public HasHighlightOnFocus.Behavior getHighlightOnFocus() {
    return super.getComponentHighlightOnFocus();
  }

  @Override
  public MaskedNumberField setHighlightOnFocus(HasHighlightOnFocus.Behavior behavior) {
    super.setComponentHighlightOnFocus(behavior);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public MaskedNumberField setHorizontalAlignment(Alignment alignment) {
    setComponentHorizontalAlignment(alignment);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public Alignment getHorizontalAlignment() {
    return getComponentHorizontalAlignment();
  }



  @Override
  public MaskedNumberField setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public MaskedNumberField setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public MaskedNumberField setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
  }

  @Override
  public boolean isEnabled() {
    return super.isComponentEnabled();
  }

  @Override
  public MaskedNumberField setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  @Override
  public MaskedNumberField setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public MaskedNumberField setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  @Override
  public MaskedNumberField addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  @Override
  public MaskedNumberField removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }



  public MaskedNumberField setExpanse(Expanse expanse) {
    super.setControlExpanse(expanse);
    return this;
  }

  public MaskedNumberField setTheme(Theme theme) {
    super.setControlTheme(theme);
    return this;
  }



  @Override
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list
                                  // of checks
  protected void onAttach() {
    super.onAttach();

    if (!this.callbacks.isEmpty()) {
      this.editModifyEventSink = new MaskedNumberFieldModifyEventSink(this);
      while (!this.callbacks.isEmpty()) {
        this.editModifyEventSink.addCallback(this.callbacks.remove(0));
      }
    }


    if (!",".equals(this.commaChar)) {
      this.setCommaCharacter(this.commaChar);
    }

    if (!".".equals(this.dotChar)) {
      this.setDotCharacter(this.dotChar);
    }

    if (!"".equals(this.editString)) {
      this.setEditString(this.editString);
    }

    if (Boolean.TRUE.equals(this.highlight)) {
      this.setHighlight(this.highlight);
    }

    if (Boolean.FALSE.equals(this.insertMode)) {
      this.setInsertMode(this.insertMode);
    }

    if (this.length != 9) {
      this.setLength(this.length);
    }

    if (this.margin != 3) {
      this.setMargin(this.margin);
    }

    if (!"-########".equals(this.mask)) {
      this.setMask(this.mask);
    }

    if (Boolean.FALSE.equals(this.negatable)) {
      this.setNegatable(this.negatable);
    }

    if (Boolean.TRUE.equals(this.pEnter)) {
      this.setPassEnter(this.pEnter);
    }

    if (Boolean.TRUE.equals(this.pTab)) {
      this.setPassTab(this.pTab);
    }

    if (this.restore != 0) {
      this.setRestore(this.restore);
    }

    if (Boolean.TRUE.equals(this.commas)) {
      this.setUseEditCommas(this.commas);
    }

    if (this.value.equals(BigDecimal.valueOf(0))) {
      this.setValue(this.value);
    }

    if (Boolean.TRUE.equals(this.readOnly)) {
      this.setReadOnly(this.readOnly);
    }

    if (Boolean.FALSE.equals(this.tabTraversable)) {
      this.setTabTraversable(this.tabTraversable);
    }

  }
}
