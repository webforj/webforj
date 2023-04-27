package org.dwcj.component.field;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import java.util.ArrayList;
import java.util.function.Consumer;

import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.HasEnable;
import org.dwcj.component.HasFocus;
import org.dwcj.component.HasReadOnly;
import org.dwcj.component.TabTraversable;
import org.dwcj.component.TextAlignable;
import org.dwcj.component.TextHighlightable;
import org.dwcj.component.field.event.FieldModifyEvent;
import org.dwcj.component.field.sink.FieldModifyEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.utilities.BBjFunctionalityHelper;


public final class Field extends AbstractDwcComponent
    implements HasReadOnly, HasFocus, TabTraversable, HasEnable, TextAlignable, TextHighlightable {

  private BBjEditBox bbjEditBox;


  public enum Expanse {
    LARGE, MEDIUM, SMALL, XLARGE, XSMALL
  }

  public enum Theme {
    DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
  }

  private ArrayList<Consumer<FieldModifyEvent>> callbacks = new ArrayList<>();
  private FieldModifyEventSink editModifyEventSink;

  private Integer maxLength = 2147483647;
  private Boolean homeDelete = false;
  private Boolean passwordVisible = false;



  public Field() {
    this("");
  }

  public Field(String text) {
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
      control = w.addEditBox(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1,
          BASISNUMBER_1, getText(), flags);
      bbjEditBox = (BBjEditBox) this.control;
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }

  }


  public Field onEditModify(Consumer<FieldModifyEvent> callback) {
    if (this.control != null) {
      if (this.editModifyEventSink == null) {
        this.editModifyEventSink = new FieldModifyEventSink(this);
      }
      this.editModifyEventSink.addCallback(callback);
    } else {
      this.callbacks.add(callback);
    }
    return this;
  }

  public String getEditType() {
    if (this.control != null) {
      return bbjEditBox.getEditType();
    }
    return "";
  }

  public Integer getMaxLength() {
    if (this.control != null) {
      return bbjEditBox.getMaxLength();
    }
    return this.maxLength;
  }

  public Boolean isPassHomeDelete() {
    if (this.control != null) {
      try {
        return bbjEditBox.getPassHomeDelete();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.homeDelete;
  }

  public String getSelectedText() {
    if (this.control != null) {
      try {
        return bbjEditBox.getSelectedText();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return null;
  }

  /* Unsure if this is the correct return type for this functionality -MH */

  /*
   * Changed this to return a single string, otherwise could not get this to properly work -MH
   */
  public String getSelection() {
    if (this.control != null) {
      try {
        return bbjEditBox.getSelection().toArray().toString();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return null;
  }



  public boolean isPasswordVisible() {
    if (this.control != null) {
      try {
        return bbjEditBox.isPasswordVisible();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.passwordVisible;
  }

  public Field select(Integer offset1, Integer offset2) {
    if (this.control != null) {
      bbjEditBox.select(offset1, offset2);
    }
    return this;
  }



  public Field setMaxLength(Integer length) {
    if (this.control != null) {
      try {
        bbjEditBox.setMaxLength(length);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.maxLength = length;
    return this;
  }

  public Field setPassHomeDelete(Boolean pass) {
    if (this.control != null) {
      try {
        bbjEditBox.setPassHomeDelete(pass);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.homeDelete = pass;
    return this;
  }

  public Field setPasswordVisible(Boolean visible) {
    if (this.control != null) {
      try {
        bbjEditBox.setPasswordVisible(visible);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.passwordVisible = visible;
    return this;
  }



  @Override
  public Boolean isReadOnly() {
    if (this.control != null) {
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
    if (this.control != null) {
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
  public Field focus() {
    super.focusComponent();
    return this;
  }

  @Override
  public Boolean isTabTraversable() {
    if (this.control != null) {
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
    if (this.control != null) {
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
    // todo: why could an exception be thrown?
    if (this.control != null) {
      try {
        bbjEditBox.setAlignment(alignment.getValue());
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
    if (this.control != null) {
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
  public Field setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
  }

  @Override 
  public boolean isEnabled(){
    return super.isComponentEnabled();
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



  public Field setExpanse(Expanse expanse) {
    super.setControlExpanse(expanse);
    return this;
  }

  public Field setTheme(Theme theme) {
    super.setControlTheme(theme);
    return this;
  }


  @Override
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp()))
      throw new IllegalAccessException("catchUp cannot be called twice");
    super.catchUp();

    if (!this.callbacks.isEmpty()) {
      this.editModifyEventSink = new FieldModifyEventSink(this);
      while (!this.callbacks.isEmpty()) {
        this.editModifyEventSink.addCallback(this.callbacks.remove(0));
      }
    }

    if (this.maxLength != 2147483647) {
      this.setMaxLength(this.maxLength);
    }

    if (Boolean.TRUE.equals(this.homeDelete)) {
      this.setPassHomeDelete(this.homeDelete);
    }

    if (Boolean.TRUE.equals(this.passwordVisible)) {
      this.setPasswordVisible(this.passwordVisible);
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
