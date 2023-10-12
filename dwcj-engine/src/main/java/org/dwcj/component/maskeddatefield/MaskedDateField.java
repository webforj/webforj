package org.dwcj.component.maskeddatefield;

import com.basis.bbj.proxies.sysgui.BBjInputD;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjNumber;
import com.basis.startup.type.sysgui.BBjColor;

import java.io.IOException;
import java.util.ArrayList;
import java.util.function.Consumer;
import javax.swing.text.Highlighter.Highlight;
import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.LegacyDwcComponent;
import org.dwcj.component.maskeddatefield.event.MaskedDateFieldModifyEvent;
import org.dwcj.component.maskeddatefield.sink.MaskedDateFieldModifyEventSink;
import org.dwcj.component.window.Window;
import org.dwcj.concern.HasHighlightOnFocus;
import org.dwcj.concern.HasHorizontalAlignment;
import org.dwcj.concern.legacy.LegacyHasEnable;
import org.dwcj.concern.legacy.LegacyHasFocus;
import org.dwcj.concern.legacy.LegacyHasReadOnly;
import org.dwcj.concern.legacy.LegacyHasTabTraversal;
import org.dwcj.utilities.BBjFunctionalityHelper;

public final class MaskedDateField extends LegacyDwcComponent
    implements LegacyHasReadOnly, LegacyHasFocus, LegacyHasTabTraversal, LegacyHasEnable,
    HasHighlightOnFocus<MaskedDateField>, HasHorizontalAlignment<MaskedDateField> {

  private BBjInputD bbjDateEditBox;

  public enum Expanse {
    LARGE, MEDIUM, SMALL, XLARGE, XSMALL
  }

  public enum Theme {
    DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
  }

  private ArrayList<Consumer<MaskedDateFieldModifyEvent>> callbacks = new ArrayList<>();
  private MaskedDateFieldModifyEventSink editModifyEventSink;

  private Boolean beep = false;
  private Integer cHeight = null;
  private Integer cWidth = null;
  private Integer caretPos = 1;
  private String editString = null;
  private Boolean highlight = false;
  private Boolean insert = false;
  private Integer length = 8;
  private String locale = "en_US";
  private Integer margin = 3;
  private String mask = "%Mz/%Dz/%Yz";
  private Boolean pEnter = false;
  private Boolean pTab = false;
  private String restore = "0";
  private Boolean plusMinus = false;
  private Boolean showWeeks = false;



  public MaskedDateField() {
    this("");
    setComponentDefaultHorizontalAlignment(Alignment.LEFT);
  }

  public MaskedDateField(String text) {
    setText(text);
    this.readOnly = false;
    this.tabTraversable = true;
  }

  @Override
  protected void onCreate(Window p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      control = w.addInputD(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1,
          BASISNUMBER_1, flags);
      bbjDateEditBox = (BBjInputD) control;
      onAttach();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public MaskedDateField onEditModify(Consumer<MaskedDateFieldModifyEvent> callback) {
    if (this.control != null) {
      if (this.editModifyEventSink == null) {
        this.editModifyEventSink = new MaskedDateFieldModifyEventSink(this);
      }
      this.editModifyEventSink.addCallback(callback);
    } else {
      this.callbacks.add(callback);
    }
    return this;
  }

  /**
   * This method pops up a calendar dialog attached to the DateEditBox control
   *
   */
  public void calendar() {
    ((BBjInputD) this.control).calendar();
  }

  /**
   * This method returns whether the DateEditBox control beeps on invalid input.
   *
   * @returns Returns whether the control will beep on invalid input (false = No Beep, true = Beep).
   *
   */
  public Boolean isBeep() {
    if (this.control != null) {
      try {
        return bbjDateEditBox.getBeep();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.beep;
  }

  /**
   * This method returns the caret position in the DateEditBox control.
   *
   * @returns Returns the position of the caret in the BBjInputD control.
   */
  public Integer getCaretPosition() {
    if (this.control != null) {
      try {
        return bbjDateEditBox.getCaretPosition();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.caretPos;
  }

  /* ==Throws an IOException - not sure if I handled this properly== */
  public String getEditString() throws IOException {
    if (this.control != null) {
      try {
        return new String(bbjDateEditBox.getEditString(), "UTF_8");
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.editString;
  }

  /**
   * This method returns the number of the last error generated in the DateEditBox control.
   *
   * @returns Returns the position of the caret in the BBjInputD control.
   */
  public Integer getError() {
    try {
      return bbjDateEditBox.getError();
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return null;
  }

  public Boolean isHighlighted() {
    if (this.control != null) {
      try {
        return bbjDateEditBox.getHighlight();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return highlight;
  }

  public Boolean isInsertMode() {
    if (this.control != null) {
      try {
        return bbjDateEditBox.getInsertMode();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.insert;
  }

  public Integer getLength() {
    if (this.control != null) {
      try {
        return bbjDateEditBox.getLength();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.length;
  }

  public String getLocale() {
    if (this.control != null) {
      return bbjDateEditBox.getLocale();
    }
    return this.locale;
  }

  public Integer getMargin() {
    if (this.control != null) {
      try {
        return bbjDateEditBox.getMargin();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.margin;
  }

  public String getMask() {
    if (this.control != null) {
      try {
        return bbjDateEditBox.getMask();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.mask;
  }

  public Boolean isPassEnter() {
    if (this.control != null) {
      try {
        return bbjDateEditBox.getPassEnter();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.pEnter;
  }

  public Boolean isPassTab() {
    if (this.control != null) {
      try {
        return bbjDateEditBox.getPassTab();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.pTab;
  }

  public Boolean isPlusMinus() {
    if (this.control != null) {
      try {
        return bbjDateEditBox.getPlusMinus();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.plusMinus;
  }

  public String getRestore() {
    if (this.control != null) {
      try {
        return bbjDateEditBox.getRestore();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.restore;
  }

  public Boolean isShowWeeks() {
    if (this.control != null) {
      try {
        return bbjDateEditBox.getShowWeeks();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.showWeeks;
  }

  /* ==Unsure if this is the correct return type== */
  public String getTodayColor() {
    try {
      return bbjDateEditBox.getTodayColor().toString();
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return "r=255,g=0,b=0";
  }

  /* ==Unsure if this is the correct return type== */
  public String getValue() {
    try {
      return bbjDateEditBox.getValue().toString();
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return "2459909";
  }

  /* ==Unsure if this is the correct return type== */
  public String getWeekdayColor() {
    try {
      return bbjDateEditBox.getWeekdayColor().toString();
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return "r=0,g=0,b=255";
  }

  /* ==Unsure if this is the correct return type== */
  public String getWeekendColor() {
    try {
      return bbjDateEditBox.getWeekendColor().toString();
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return "r=0,g=128,b=0";
  }



  public Boolean isValid() {
    if (this.control != null) {
      try {
        return bbjDateEditBox.isValid();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return true;
  }

  public MaskedDateField restore() {
    if (this.control != null) {
      try {
        bbjDateEditBox.restore();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public MaskedDateField selectAll() {
    if (this.control != null) {
      try {
        bbjDateEditBox.selectAll();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }



  public MaskedDateField setBeep(Boolean beep) {
    App.consoleLog("In Beep");
    this.beep = beep;
    if (this.control != null) {
      try {
        App.consoleLog(this.beep.toString());
        bbjDateEditBox.setBeep(beep);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public MaskedDateField setCalendarSize(int width, int height) {
    this.cWidth = width;
    this.cHeight = height;
    if (this.control != null) {
      try {
        bbjDateEditBox.setCalendarSize(width, height);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public MaskedDateField setCaretPosition(int position) {
    this.caretPos = position;
    if (this.control != null) {
      try {
        bbjDateEditBox.setCaretPosition(position);
      } catch (BBjException e) {
        Environment.logError(e);
      }

    }
    return this;
  }



  public MaskedDateField setEditString(String edit) {
    this.editString = edit;
    if (this.control != null) {
      try {
        bbjDateEditBox.setEditString(edit.getBytes());
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public MaskedDateField setHighlight(Boolean highlight) {
    this.highlight = highlight;
    if (this.control != null) {
      try {
        bbjDateEditBox.setHighlight(highlight);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }


  public MaskedDateField setInsertMode(Boolean insert) {
    this.insert = insert;
    if (this.control != null) {
      try {
        bbjDateEditBox.setInsertMode(insert);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public MaskedDateField setLength(Integer length) {
    this.length = length;
    if (control != null) {
      try {
        bbjDateEditBox.setLength(length);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }


  public MaskedDateField setLocale(String locale) {
    this.locale = locale;
    if (this.control != null) {
      bbjDateEditBox.setLocale(locale);
    }
    return this;
  }

  public MaskedDateField setMargin(Integer marginWidth) {
    this.margin = marginWidth;
    if (this.control != null) {
      try {
        bbjDateEditBox.setMargin(marginWidth);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public MaskedDateField setMask(String mask) {
    this.mask = mask;
    if (this.control != null) {
      try {
        bbjDateEditBox.setMask(mask);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public MaskedDateField setPassEnter(Boolean pass) {
    this.pEnter = pass;
    if (this.control != null) {
      try {
        bbjDateEditBox.setPassEnter(pass);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public MaskedDateField setPassTab(Boolean pass) {
    this.pTab = pass;
    if (this.control != null) {
      try {
        bbjDateEditBox.setPassTab(pass);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public MaskedDateField setRestore(String restore) {
    this.restore = restore;
    if (this.control != null) {
      try {
        bbjDateEditBox.setRestore(restore);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public MaskedDateField setPlusMinus(Boolean plusMinus) {
    this.plusMinus = plusMinus;
    if (control != null) {
      try {
        bbjDateEditBox.setPlusMinus(plusMinus);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public MaskedDateField setShowWeeks(boolean showWeeks) {
    this.showWeeks = showWeeks;
    if (this.control != null) {
      try {
        bbjDateEditBox.setShowWeeks(showWeeks);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  /*
   * ==Unsure if simply casting this object to BBjColor is acceptable, likely need to control/check
   * input before passing?== -MH
   */
  public MaskedDateField setTodayColor(Object color) {
    try {
      bbjDateEditBox.setTodayColor((BBjColor) color);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return this;
  }

  /*
   * ==Unsure if simply casting this object to BBjNumber is acceptable, likely need to control/check
   * input before passing?== -MH
   */
  public MaskedDateField setValue(Object value) {
    try {
      bbjDateEditBox.setValue((BBjNumber) value);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return this;
  }

  /*
   * ==Unsure if simply casting this object to BBjColor is acceptable, likely need to control/check
   * input before passing?== -MH
   */
  public MaskedDateField setWeekdayColor(Object color) {
    try {
      bbjDateEditBox.setWeekdayColor((BBjColor) color);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return this;
  }

  /*
   * ==Unsure if simply casting this object to BBjColor is acceptable, likely need to control/check
   * input before passing?== -MH
   */
  public MaskedDateField setWeekendColor(Object color) {
    try {
      bbjDateEditBox.setWeekendColor((BBjColor) color);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return this;
  }



  @Override
  public Boolean isReadOnly() {
    if (this.control != null) {
      try {
        return !bbjDateEditBox.isEditable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.readOnly;
  }

  @Override
  public MaskedDateField setReadOnly(Boolean editable) {
    if (this.control != null) {
      try {
        bbjDateEditBox.setEditable(!editable);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.readOnly = editable;
    return this;
  }

  @Override
  public MaskedDateField focus() {
    super.focusComponent();
    return this;
  }

  @Override
  public Boolean isTabTraversable() {
    if (this.control != null) {
      try {
        bbjDateEditBox.isTabTraversable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.tabTraversable;
  }

  @Override
  public MaskedDateField setTabTraversable(Boolean traverse) {
    if (this.control != null) {
      try {
        bbjDateEditBox.setTabTraversable(traverse);
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
  public MaskedDateField setHighlightOnFocus(HasHighlightOnFocus.Behavior behavior) {
    super.setComponentHighlightOnFocus(behavior);
    return this;
  }

  @Override
  public Alignment getHorizontalAlignment() {
    return getComponentHorizontalAlignment();
  }

  @Override
  public MaskedDateField setHorizontalAlignment(Alignment alignment) {
    super.setComponentHorizontalAlignment(alignment);
    return this;
  }



  @Override
  public MaskedDateField setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public MaskedDateField setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public MaskedDateField setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
  }

  @Override
  public boolean isEnabled() {
    return super.isComponentEnabled();
  }

  @Override
  public MaskedDateField setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  @Override
  public MaskedDateField setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public MaskedDateField setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  @Override
  public MaskedDateField addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  @Override
  public MaskedDateField removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }



  public MaskedDateField setExpanse(Expanse expanse) {
    super.setControlExpanse(expanse);
    return this;
  }

  public MaskedDateField setTheme(Theme theme) {
    super.setControlTheme(theme);
    return this;
  }

  @Override
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list
                                  // of checks
  protected void onAttach() {
    super.onAttach();

    if (!this.callbacks.isEmpty()) {
      this.editModifyEventSink = new MaskedDateFieldModifyEventSink(this);
      while (!this.callbacks.isEmpty()) {
        this.editModifyEventSink.addCallback(this.callbacks.remove(0));
      }
    }

    if (Boolean.TRUE.equals(this.beep)) {
      this.setBeep(this.beep);
    }

    if (this.caretPos != 1) {
      this.setCaretPosition(this.caretPos);
    }

    if (this.cHeight != null && this.cWidth != null) {
      this.setCalendarSize(this.cHeight, this.cWidth);
    }

    if (this.editString != null) {
      this.setEditString(this.editString);
    }

    if (Boolean.TRUE.equals(this.highlight)) {
      this.setHighlight(this.highlight);
    }

    if (Boolean.TRUE.equals(this.insert)) {
      this.setInsertMode(this.insert);
    }

    if (this.length != 8) {
      this.setLength(this.length);
    }

    if (!this.locale.equals("en_US")) {
      this.setLocale(this.locale);
    }

    if (this.margin != 3) {
      this.setMargin(this.margin);
    }

    if (!this.mask.equals("%Mz/%Dz/%Yz")) {
      this.setMask(this.mask);
    }

    if (Boolean.TRUE.equals(this.pEnter)) {
      this.setPassEnter(this.pEnter);
    }

    if (Boolean.TRUE.equals(this.pTab)) {
      this.setPassTab(this.pTab);
    }

    if (!"0".equals(this.restore)) {
      this.setRestore(this.restore);
    }

    if (Boolean.TRUE.equals(this.plusMinus)) {
      this.setPlusMinus(this.plusMinus);
    }

    if (Boolean.TRUE.equals(this.showWeeks)) {
      this.setShowWeeks(this.showWeeks);
    }

    if (Boolean.TRUE.equals(this.readOnly)) {
      this.setReadOnly(this.readOnly);
    }

    if (Boolean.FALSE.equals(this.tabTraversable)) {
      this.setTabTraversable(this.tabTraversable);
    }

  }
}
