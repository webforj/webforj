package org.dwcj.component.maskedtextfield;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.function.Consumer;
import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.LegacyDwcComponent;
import org.dwcj.component.maskedtextfield.event.MaskedTextFieldModifyEvent;
import org.dwcj.component.maskedtextfield.sink.MaskedTextFieldModifyEventSink;
import org.dwcj.component.text.Label;
import org.dwcj.component.window.Window;
import org.dwcj.concern.HasHighlightOnFocus;
import org.dwcj.concern.HasHorizontalAlignment;
import org.dwcj.concern.legacy.LegacyHasEnable;
import org.dwcj.concern.legacy.LegacyHasFocus;
import org.dwcj.concern.legacy.LegacyHasReadOnly;
import org.dwcj.concern.legacy.LegacyHasTabTraversal;
import org.dwcj.utilities.BBjFunctionalityHelper;
import com.basis.bbj.proxies.sysgui.BBjInputE;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

public final class MaskedTextField extends LegacyDwcComponent
    implements LegacyHasReadOnly, LegacyHasFocus, LegacyHasTabTraversal,
    HasHorizontalAlignment<MaskedTextField>, HasHighlightOnFocus<MaskedTextField>, LegacyHasEnable {


  private BBjInputE bbjInputE;

  public enum Expanse {
    LARGE, MEDIUM, SMALL, XLARGE, XSMALL
  }

  public enum Theme {
    DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
  }


  private ArrayList<Consumer<MaskedTextFieldModifyEvent>> callbacks = new ArrayList<>();
  private MaskedTextFieldModifyEventSink editModifyEventSink;

  private Integer caretPos = 1;
  private String editString = "";
  private Boolean highlight = false;
  private Boolean insert = false;
  private Integer length = null;
  private Integer margin = 7;
  private String mask =
      "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
  private String pad = " ";
  private Boolean passEnter = false;
  private Boolean passTab = false;
  private String restore = "";



  public MaskedTextField() {
    this("");
  }

  public MaskedTextField(String text) {
    setText(text);
    this.readOnly = false;
    this.tabTraversable = true;
    setComponentDefaultHorizontalAlignment(Alignment.LEFT);
  }

  @Override
  protected void onCreate(Window p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      control = w.addInputE(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1,
          BASISNUMBER_1, flags);
      bbjInputE = (BBjInputE) control;
      onAttach();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public MaskedTextField onEditModify(Consumer<MaskedTextFieldModifyEvent> callback) {

    if (this.control != null) {
      if (this.editModifyEventSink == null) {
        this.editModifyEventSink = new MaskedTextFieldModifyEventSink(this);
      }
      this.editModifyEventSink.addCallback(callback);
    } else {
      this.callbacks.add(callback);
    }
    return this;
  }


  public Integer getCaretPos() {
    if (this.control != null) {
      try {
        return bbjInputE.getCaretPosition();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.caretPos;
  }

  public Integer getError() {
    if (this.control != null) {
      try {
        return bbjInputE.getError();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return null;
  }

  public String getEditString() {
    if (this.control != null) {
      try {
        return new String(bbjInputE.getEditString(), StandardCharsets.UTF_8);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.editString;
  }

  public Boolean isHighlight() {
    if (this.control != null) {
      try {
        return bbjInputE.getHighlight();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.highlight;
  }

  public Boolean isInsertMode() {
    if (this.control != null) {
      try {
        return bbjInputE.getInsertMode();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.insert;
  }

  public Integer getLength() {
    if (this.control != null) {
      try {
        bbjInputE.getLength();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.length;
  }

  public Integer getMargin() {
    if (this.control != null) {
      try {
        return bbjInputE.getMargin();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return -1;
  }

  public String getMask() {
    if (this.control != null) {
      try {
        return bbjInputE.getMask();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.mask;
  }


  public String getPadCharacter() {
    if (this.control != null) {
      try {
        return bbjInputE.getPadCharacter();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.pad;
  }

  public Boolean isPassEnter() {
    if (this.control != null) {
      try {
        return bbjInputE.getPassEnter();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.passEnter;
  }

  public Boolean isPassTab() {
    if (this.control != null) {
      try {
        return bbjInputE.getPassTab();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.passTab;
  }

  public String getRestore() {
    if (this.control != null) {
      try {
        return bbjInputE.getRestore();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.restore;
  }



  public MaskedTextField restore() {
    if (this.control != null) {
      try {
        bbjInputE.restore();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  public MaskedTextField selectAll() {
    if (this.control != null) {
      try {
        bbjInputE.selectAll();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }


  public MaskedTextField setCaretPos(Integer position) {
    if (this.control != null) {
      try {
        bbjInputE.setCaretPosition(position);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.caretPos = position;
    return this;
  }

  public MaskedTextField setEditString(String edit) {
    if (this.control != null) {
      try {
        bbjInputE.setEditString(edit.getBytes(StandardCharsets.UTF_8));
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.editString = edit;
    return this;
  }

  public MaskedTextField setHighlight(Boolean highlight) {
    if (this.control != null) {
      try {
        bbjInputE.setHighlight(highlight);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.highlight = highlight;
    return this;
  }

  public MaskedTextField setInsertMode(Boolean insert) {
    if (this.control != null) {
      try {
        bbjInputE.setInsertMode(insert);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.insert = insert;
    return this;
  }

  public MaskedTextField setLength(Integer len) {
    if (this.control != null) {
      try {
        bbjInputE.setLength(len);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.length = len;
    return this;
  }

  public MaskedTextField setMargin(Integer marginWidth) {
    if (this.control != null) {
      try {
        bbjInputE.setMargin(marginWidth);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.margin = marginWidth;
    return this;
  }

  public MaskedTextField setMask(String mask) {
    if (control != null) {
      try {
        ((BBjInputE) control).setMask(mask);
      } catch (BBjException e) {
        App.consoleLog(e.getMessage());
        throw new RuntimeException(e);
      }
    }
    this.mask = mask;
    return this;
  }

  public MaskedTextField setPadCharacter(String pad) {
    if (this.control != null) {
      try {
        bbjInputE.setPadCharacter(pad);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.pad = pad;
    return this;
  }

  public MaskedTextField setPassEnter(Boolean pass) {
    if (this.control != null) {
      try {
        bbjInputE.setPassEnter(pass);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.passEnter = pass;
    return this;
  }

  public MaskedTextField setPassTab(Boolean pass) {
    if (this.control != null) {
      try {
        bbjInputE.setPassTab(pass);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.passTab = pass;
    return this;
  }

  public MaskedTextField setRestore(String restore) {
    if (this.control != null) {
      try {
        bbjInputE.setRestore(restore);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.restore = restore;
    return this;
  }



  @Override
  public Boolean isReadOnly() {
    try {
      return !bbjInputE.isEditable();
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return this.readOnly;
  }

  @Override
  public MaskedTextField setReadOnly(Boolean editable) {
    try {
      bbjInputE.setEditable(!editable);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return this;
  }

  @Override
  public MaskedTextField focus() {
    super.focusComponent();
    return this;
  }

  @Override
  public Boolean isTabTraversable() {
    if (this.control != null) {
      try {
        bbjInputE.isTabTraversable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.tabTraversable;
  }

  @Override
  public MaskedTextField setTabTraversable(Boolean traversable) {
    if (this.control != null) {
      try {
        bbjInputE.setTabTraversable(traversable);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.tabTraversable = traversable;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public MaskedTextField setHorizontalAlignment(Alignment alignment) {
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
  public HasHighlightOnFocus.Behavior getHighlightOnFocus() {
    return super.getComponentHighlightOnFocus();
  }

  @Override
  public MaskedTextField setHighlightOnFocus(HasHighlightOnFocus.Behavior behavior) {
    super.setComponentHighlightOnFocus(behavior);
    return this;
  }



  @Override
  public MaskedTextField setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public MaskedTextField setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public MaskedTextField setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
  }

  @Override
  public boolean isEnabled() {
    return super.isComponentEnabled();
  }

  @Override
  public MaskedTextField setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  @Override
  public MaskedTextField setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public MaskedTextField setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  @Override
  public MaskedTextField addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  @Override
  public MaskedTextField removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }



  public MaskedTextField setExpanse(Expanse expanse) {
    super.setControlExpanse(expanse);
    return this;
  }


  public MaskedTextField setTheme(Theme theme) {
    super.setControlTheme(theme);
    return this;
  }



  @Override
  protected void onAttach() {
    super.onAttach();


    if (!this.callbacks.isEmpty()) {
      this.editModifyEventSink = new MaskedTextFieldModifyEventSink(this);
      while (!this.callbacks.isEmpty()) {
        this.editModifyEventSink.addCallback(this.callbacks.remove(0));
      }
    }


    if (this.caretPos != 1) {
      this.setCaretPos(this.caretPos);
    }

    if (!"".equals(this.editString)) {
      this.setEditString(this.editString);
    }

    if (Boolean.TRUE.equals(this.highlight)) {
      this.setHighlight(this.highlight);
    }

    if (Boolean.TRUE.equals(this.insert)) {
      this.setInsertMode(this.insert);
    }

    if (this.length != null) {
      this.setLength(this.length);
    }

    if (this.margin != 7) {
      this.setMargin(this.margin);
    }

    if (!"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
        .equals(this.mask)) {
      this.setMask(this.mask);
    }

    if (!" ".equals(this.pad)) {
      this.setPadCharacter(this.pad);
    }

    if (Boolean.TRUE.equals(this.passEnter)) {
      this.setPassEnter(this.passEnter);
    }

    if (Boolean.TRUE.equals(this.passTab)) {
      this.setPassTab(this.passTab);
    }

    if (!"".equals(this.restore)) {
      this.setRestore(this.restore);
    }



    if (Boolean.TRUE.equals(this.readOnly)) {
      this.setReadOnly(this.readOnly);
    }

    if (Boolean.FALSE.equals(this.tabTraversable)) {
      this.setTabTraversable(this.tabTraversable);
    }
  }


}
