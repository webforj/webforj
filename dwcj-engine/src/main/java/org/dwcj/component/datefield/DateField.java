package org.dwcj.component.datefield;

import com.basis.bbj.proxies.sysgui.BBjInputD;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjNumber;
import com.basis.startup.type.sysgui.BBjColor;

import java.io.IOException;
import java.util.ArrayList;
import java.util.function.Consumer;

import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.HasEnable;
import org.dwcj.component.HasFocus;
import org.dwcj.component.HasReadOnly;
import org.dwcj.component.TabTraversable;
import org.dwcj.component.TextAlignable;
import org.dwcj.component.TextHighlightable;
import org.dwcj.component.datefield.event.DateFieldModifyEvent;
import org.dwcj.component.datefield.sink.DateFieldModifyEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.utilities.BBjFunctionalityHelper;

public final class DateField extends AbstractDwcComponent
    implements HasReadOnly, HasFocus, TabTraversable, HasEnable, TextHighlightable, TextAlignable {

  private BBjInputD bbjDateEditBox;

  public enum Expanse {
    LARGE, MEDIUM, SMALL, XLARGE, XSMALL
  }

  public enum Theme {
    DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
  }

  private ArrayList<Consumer<DateFieldModifyEvent>> callbacks = new ArrayList<>();
  private DateFieldModifyEventSink editModifyEventSink;

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



  public DateField() {
    this("");
  }

  public DateField(String text) {
    setText(text);
    this.readOnly = false;
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
      ctrl = w.addInputD(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1,
          BASISNUMBER_1, flags);
      bbjDateEditBox = (BBjInputD) ctrl;
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public DateField onEditModify(Consumer<DateFieldModifyEvent> callback) {
    if (this.ctrl != null) {
      if (this.editModifyEventSink == null) {
        this.editModifyEventSink = new DateFieldModifyEventSink(this);
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
    ((BBjInputD) this.ctrl).calendar();
  }

  /**
   * This method returns whether the DateEditBox control beeps on invalid input.
   *
   * @returns Returns whether the control will beep on invalid input (false = No Beep, true = Beep).
   *
   */
  public Boolean isBeep() {
    if (this.ctrl != null) {
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
    if (this.ctrl != null) {
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
    if (this.ctrl != null) {
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
    if (this.ctrl != null) {
      try {
        return bbjDateEditBox.getHighlight();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return highlight;
  }

  public Boolean isInsertMode() {
    if (this.ctrl != null) {
      try {
        return bbjDateEditBox.getInsertMode();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.insert;
  }

  public Integer getLength() {
    if (this.ctrl != null) {
      try {
        return bbjDateEditBox.getLength();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.length;
  }

  public String getLocale() {
    if (this.ctrl != null) {
      return bbjDateEditBox.getLocale();
    }
    return this.locale;
  }

  public Integer getMargin() {
    if (this.ctrl != null) {
      try {
        return bbjDateEditBox.getMargin();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.margin;
  }

  public String getMask() {
    if (this.ctrl != null) {
      try {
        return bbjDateEditBox.getMask();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.mask;
  }

  public Boolean isPassEnter() {
    if (this.ctrl != null) {
      try {
        return bbjDateEditBox.getPassEnter();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.pEnter;
  }

  public Boolean isPassTab() {
    if (this.ctrl != null) {
      try {
        return bbjDateEditBox.getPassTab();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.pTab;
  }

  public Boolean isPlusMinus() {
    if (this.ctrl != null) {
      try {
        return bbjDateEditBox.getPlusMinus();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.plusMinus;
  }

  public String getRestore() {
    if (this.ctrl != null) {
      try {
        return bbjDateEditBox.getRestore();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.restore;
  }

  public Boolean isShowWeeks() {
    if (this.ctrl != null) {
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
    if (this.ctrl != null) {
      try {
        return bbjDateEditBox.isValid();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return true;
  }

  public DateField restore() {
    if (this.ctrl != null) {
      try {
        bbjDateEditBox.restore();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public DateField selectAll() {
    if (this.ctrl != null) {
      try {
        bbjDateEditBox.selectAll();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }



  public DateField setBeep(Boolean beep) {
    App.consoleLog("In Beep");
    this.beep = beep;
    if (this.ctrl != null) {
      try {
        App.consoleLog(this.beep.toString());
        bbjDateEditBox.setBeep(beep);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public DateField setCalendarSize(int width, int height) {
    this.cWidth = width;
    this.cHeight = height;
    if (this.ctrl != null) {
      try {
        bbjDateEditBox.setCalendarSize(width, height);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public DateField setCaretPosition(int position) {
    this.caretPos = position;
    if (this.ctrl != null) {
      try {
        bbjDateEditBox.setCaretPosition(position);
      } catch (BBjException e) {
        Environment.logError(e);
      }

    }
    return this;
  }



  public DateField setEditString(String edit) {
    this.editString = edit;
    if (this.ctrl != null) {
      try {
        bbjDateEditBox.setEditString(edit.getBytes());
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public DateField setHighlight(Boolean highlight) {
    this.highlight = highlight;
    if (this.ctrl != null) {
      try {
        bbjDateEditBox.setHighlight(highlight);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }


  public DateField setInsertMode(Boolean insert) {
    this.insert = insert;
    if (this.ctrl != null) {
      try {
        bbjDateEditBox.setInsertMode(insert);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public DateField setLength(Integer length) {
    this.length = length;
    if (ctrl != null) {
      try {
        bbjDateEditBox.setLength(length);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }


  public DateField setLocale(String locale) {
    this.locale = locale;
    if (this.ctrl != null) {
      bbjDateEditBox.setLocale(locale);
    }
    return this;
  }

  public DateField setMargin(Integer marginWidth) {
    this.margin = marginWidth;
    if (this.ctrl != null) {
      try {
        bbjDateEditBox.setMargin(marginWidth);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public DateField setMask(String mask) {
    this.mask = mask;
    if (this.ctrl != null) {
      try {
        bbjDateEditBox.setMask(mask);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public DateField setPassEnter(Boolean pass) {
    this.pEnter = pass;
    if (this.ctrl != null) {
      try {
        bbjDateEditBox.setPassEnter(pass);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public DateField setPassTab(Boolean pass) {
    this.pTab = pass;
    if (this.ctrl != null) {
      try {
        bbjDateEditBox.setPassTab(pass);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public DateField setRestore(String restore) {
    this.restore = restore;
    if (this.ctrl != null) {
      try {
        bbjDateEditBox.setRestore(restore);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public DateField setPlusMinus(Boolean plusMinus) {
    this.plusMinus = plusMinus;
    if (ctrl != null) {
      try {
        bbjDateEditBox.setPlusMinus(plusMinus);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public DateField setShowWeeks(boolean showWeeks) {
    this.showWeeks = showWeeks;
    if (this.ctrl != null) {
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
  public DateField setTodayColor(Object color) {
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
  public DateField setValue(Object value) {
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
  public DateField setWeekdayColor(Object color) {
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
  public DateField setWeekendColor(Object color) {
    try {
      bbjDateEditBox.setWeekendColor((BBjColor) color);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return this;
  }



  @Override
  public Boolean isReadOnly() {
    if (this.ctrl != null) {
      try {
        return !bbjDateEditBox.isEditable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.readOnly;
  }

  @Override
  public DateField setReadOnly(Boolean editable) {
    if (this.ctrl != null) {
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
  public DateField focus() {
    super.focusComponent();
    return this;
  }

  @Override
  public Boolean isTabTraversable() {
    if (this.ctrl != null) {
      try {
        bbjDateEditBox.isTabTraversable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.tabTraversable;
  }

  @Override
  public DateField setTabTraversable(Boolean traverse) {
    if (this.ctrl != null) {
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
  public Highlight getHighlightOnFocus() {
    return this.textHighlight;
  }

  @Override
  public DateField setHighlightOnFocus(Highlight highlight) {
    if (this.ctrl != null) {
      try {
        bbjDateEditBox.setHighlightOnFocus(highlight.highlightType);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.textHighlight = highlight;
    return this;
  }

  @Override
  public Alignment getTextAlignment() {
    return this.textAlignment;
  }

  @Override
  public DateField setTextAlignment(Alignment alignment) {
    if (this.ctrl != null) {
      try {
        bbjDateEditBox.setAlignment(alignment.getValue());
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.textAlignment = alignment;
    return this;
  }



  @Override
  public DateField setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public DateField setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public DateField setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
  }

  @Override
  public boolean isEnabled() {
    return super.isComponentEnabled();
  }

  @Override
  public DateField setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  @Override
  public DateField setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public DateField setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  @Override
  public DateField addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  @Override
  public DateField removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }



  public DateField setExpanse(Expanse expanse) {
    super.setControlExpanse(expanse);
    return this;
  }

  public DateField setTheme(Theme theme) {
    super.setControlTheme(theme);
    return this;
  }

  @Override
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list
                                  // of checks
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp()))
      throw new IllegalAccessException("catchUp cannot be called twice");
    super.catchUp();

    if (!this.callbacks.isEmpty()) {
      this.editModifyEventSink = new DateFieldModifyEventSink(this);
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

    if (this.textAlignment != Alignment.LEFT) {
      this.setTextAlignment(this.textAlignment);
    }

    if (this.textHighlight != Highlight.HIGHLIGHT_NONE) {
      this.setHighlightOnFocus(this.textHighlight);
    }

  }
}
