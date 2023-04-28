package org.dwcj.component.radiobutton;

import com.basis.bbj.proxies.sysgui.BBjRadioButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.HasEnable;
import org.dwcj.component.HasFocus;
import org.dwcj.component.HasReadOnly;
import org.dwcj.component.TabTraversable;
import org.dwcj.component.radiobutton.event.RadioButtonCheckEvent;
import org.dwcj.component.radiobutton.sink.RadioButtonCheckEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.utilities.BBjFunctionalityHelper;
import java.util.ArrayList;
import java.util.function.Consumer;

public final class RadioButton extends AbstractDwcComponent
    implements HasReadOnly, HasFocus, TabTraversable, HasEnable {

  private BBjRadioButton bbjRadioButton;

  public enum Expanse {
    LARGE, MEDIUM, SMALL, XLARGE, XSMALL
  }

  public enum HorizontalTextPosition {
    RIGHT(4), LEFT(2), CENTER(0), LEADING(10), TRAILING(11);

    public final Integer position;

    private HorizontalTextPosition(Integer position) {
      this.position = position;
    }
  }

  private ArrayList<Consumer<RadioButtonCheckEvent>> callbacks = new ArrayList<>();
  private RadioButtonCheckEventSink checkEventSink;
  private Boolean selected = false;
  private HorizontalTextPosition horizontalTextPosition = HorizontalTextPosition.RIGHT;

  public RadioButton() {
    this.readOnly = false;
    this.tabTraversable = true;
  }

  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      ctrl = w.addRadioButton(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1,
          BASISNUMBER_1, BASISNUMBER_1, "", flags);
      bbjRadioButton = (BBjRadioButton) ctrl;
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  /**
   * register an event callback for a checkOn or checkOff event
   *
   * @param callback A method to receive the onCheck event
   * @return the control itself
   */
  public RadioButton onChange(Consumer<RadioButtonCheckEvent> callback) {
    if (this.ctrl != null) {
      if (this.checkEventSink == null) {
        this.checkEventSink = new RadioButtonCheckEventSink(this);
      }
      this.checkEventSink.addCallback(callback);
    } else {
      this.callbacks.add(callback);
    }
    return this;
  }

  /**
   * Returns the ID of a button - IDs are assigned when a control is added to a panel, not before.
   *
   * @return The ID of the control which has been added to a panel.
   */
  public Integer getButtonID() {
    if (this.ctrl != null) {
      try {
        return bbjRadioButton.getID();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    App.consoleError(
        "ID cannot be fetched as control does not yet exist. Please add control to a window first");
    return null;
  }

  /**
   * Gets the position of the text alignment in relation to the radio button
   *
   * @return Enum value of the horizontal text position for the control.
   */
  public HorizontalTextPosition getHorizontalTextPosition() {
    if (this.ctrl != null) {
      return this.horizontalTextPosition;
    }
    return this.horizontalTextPosition;
  }

  public RadioButton setHorizontalTextPosition(HorizontalTextPosition position) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setHorizontalTextPosition(position.position);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.horizontalTextPosition = position;
    return this;
  }

  /**
   * Returns true if the radio button is selected, false otherwise.
   *
   * @return Boolean value representing the radio button's selection value
   */
  public Boolean isSelected() {
    if (this.ctrl != null) {
      try {
        return bbjRadioButton.isSelected();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.selected;
  }

  /**
   * Sets the selection value of the radio button - true if the button should be selected, false if
   * not.
   *
   * @param selected Boolean for desired selection state of the button.
   * @return The control itself
   */
  public RadioButton setSelected(boolean selected) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setSelected(selected);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.selected = selected;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Boolean isReadOnly() {
    if (this.ctrl != null) {
      try {
        return !bbjRadioButton.isEditable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.readOnly;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton setReadOnly(Boolean editable) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setEditable(!editable);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  @Override
  public RadioButton focus() {
    super.focusComponent();
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Boolean isTabTraversable() {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.isTabTraversable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.tabTraversable;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton setTabTraversable(Boolean traverse) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setTabTraversable(traverse);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.tabTraversable = traverse;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton setText(String text) {
    super.setText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public RadioButton setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
  }

  @Override
  public boolean isEnabled() {
    return super.isComponentEnabled();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }

  /**
   * Sets the expanse of the radio button from the enum with control-specific applicable expanse
   * values
   *
   * <pre>
   * {@code
   * RadioButton button = new RadioButton().setExpanse(RadioButton.Expanse.LARGE);
   * }
   * </pre>
   *
   * @param expanse The enum value representing the desired expanse
   * @return
   */
  public RadioButton setExpanse(Expanse expanse) {
    super.setControlExpanse(expanse);
    return this;
  }

  @Override
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list
                                  // of checks
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp()))
      throw new IllegalAccessException("catchUp cannot be called twice");
    super.catchUp();

    if (Boolean.TRUE.equals(this.selected)) {
      this.setSelected(this.selected);
    }

    if (!this.callbacks.isEmpty()) {
      this.checkEventSink = new RadioButtonCheckEventSink(this);
      while (!this.callbacks.isEmpty()) {
        this.checkEventSink.addCallback(this.callbacks.remove(0));
      }
    }

    if (this.horizontalTextPosition != HorizontalTextPosition.RIGHT) {
      try {
        bbjRadioButton.setHorizontalTextPosition(horizontalTextPosition.position);
      } catch (BBjException e) {
        Environment.logError(e);
      }
      this.setHorizontalTextPosition(this.horizontalTextPosition);
    }

    if (Boolean.TRUE.equals(this.readOnly)) {
      this.setReadOnly(true);
    }

    if (Boolean.FALSE.equals(this.tabTraversable)) {
      this.setTabTraversable(this.tabTraversable);
    }

    if (Boolean.FALSE.equals(this.selected)) {
      this.setSelected(this.selected);
    }

  }

}
