package org.dwcj.component.button;

import com.basis.bbj.proxies.sysgui.BBjButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import java.io.File;
import org.dwcj.Environment;
import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.HasEnable;
import org.dwcj.component.HasFocus;
import org.dwcj.component.TabTraversable;
import org.dwcj.component.TextAlignable;
import org.dwcj.component.button.event.ButtonClickEvent;
import org.dwcj.component.button.sink.ButtonClickEventSink;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.EventManager;
import org.dwcj.component.event.HasMouseEnter;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.MouseExitEvent;
import org.dwcj.component.event.RightMouseDownEvent;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.dwcj.component.event.sink.RightMouseDownEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.BBjFunctionalityHelper;
import org.dwcj.utilities.ImageUtil;


/**
 * A Push Button.
 */
public final class Button extends AbstractDwcComponent
    implements HasFocus, TabTraversable, TextAlignable, HasEnable, HasMouseEnter {

  /**
   * Vertical alignment options for the Button component.
   */
  public enum TextVerticalAlignment {
    TOP(1), CENTER(0), BOTTOM(3);

    public final Integer alignment;

    private TextVerticalAlignment(Integer alignment) {
      this.alignment = alignment;
    }
  }


  private BBjButton bbjButton;
  private TextVerticalAlignment verticalAlignment = TextVerticalAlignment.CENTER;
  private Boolean disableOnClick = false;

  private final EventManager<Button> eventManager = new EventManager<>(this);


  public Button() {
    this("");
  }

  /**
   * Parameterized button constructor, accepts a string as an argument which will be the initial
   * text displayed on the button.
   *
   * @param text String value for initial button text
   */
  public Button(String text) {
    super.setText(text);
    this.tabTraversable = true;
    this.textAlignment = Alignment.MIDDLE;
    registerEvents();
  }

  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      control = w.addButton(super.getText(), flags);
      this.bbjButton = (BBjButton) control;
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  private void registerEvents() {
    this.eventManager.addEvent(ButtonClickEventSink.class, ButtonClickEvent.class);
    this.eventManager.addEvent(MouseExitEventSink.class, MouseExitEvent.class);
    this.eventManager.addEvent(RightMouseDownEventSink.class, RightMouseDownEvent.class);
    this.eventManager.addEvent(MouseEnterEventSink.class, MouseEnterEvent.class);
  }

  public void setImage(String path) {
    this.setImage(new File(path));
  }

  public void setImage(File file) {
    if (this.bbjButton != null) {
      try {
        this.bbjButton.setImage(ImageUtil.convertFileToBBjImage(file));
      } catch (BBjException e) {
        e.printStackTrace();
      }
    }
  }

  /**
   * Adds a click event for the Button component.
   *
   * @param listener The event
   * @return The component itself
   */
  public Button addClickListener(EventListener<ButtonClickEvent> listener) {
    this.eventManager.addEventListener(ButtonClickEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addClickEvent method.
   *
   * @see Button#removeClickEvent(EventListener)
   * @param listener A method to receive the click event
   * @return the control itself
   */

  public Button onClick(EventListener<ButtonClickEvent> listener) {
    return addClickListener(listener);
  }

  /**
   * Removes a click event from the Button component.
   *
   * @param listener The event to be removed
   * @return The component itself
   */
  public Button removeClickListener(EventListener<ButtonClickEvent> listener) {
    this.eventManager.removeEventListener(ButtonClickEvent.class, listener);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public Button addMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    this.eventManager.addEventListener(MouseEnterEvent.class, listener);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public Button onMouseEnter(EventListener<MouseEnterEvent> listener) {
    return addMouseEnterListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public Button removeMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    this.eventManager.removeEventListener(MouseEnterEvent.class, listener);
    return this;
  }

  /**
   * Adds a MouseExit event for the Button component.
   *
   * @param listener the event listener to be added
   * @return The Button itself
   */
  public Button addMouseExitListener(EventListener<MouseExitEvent> listener) {
    this.eventManager.addEventListener(MouseExitEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addMouseExitListener method.
   *
   * @see Button#addMouseExitListener(EventListener)
   * @param listener the event listener to be added
   * @return The Button itself
   */
  public Button onMouseExit(EventListener<MouseExitEvent> listener) {
    return addMouseExitListener(listener);
  }

  /**
   * Removes a MouseExit event from the Button component.
   *
   * @param listener the event listener to be removed
   * @return The Button itself
   */
  public Button removeMouseExitListener(EventListener<MouseExitEvent> listener) {
    this.eventManager.removeEventListener(MouseExitEvent.class, listener);
    return this;
  }

  /**
   * Adds a MouseExit event for the Button component.
   *
   * @param listener the event listener to be added
   * @return The Button itself
   */
  public Button addRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    this.eventManager.addEventListener(RightMouseDownEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addRightMouseDownListener method.
   *
   * @see Button#addRightMouseDownListener(EventListener)
   * @param listener the event listener to be added
   * @return The Button itself
   */
  public Button onRightMouseDown(EventListener<RightMouseDownEvent> listener) {
    return addRightMouseDownListener(listener);
  }

  /**
   * Removes a RightMouseDown event from the Button component.
   *
   * @param listener the event listener to be removed
   * @return The Button itself
   */
  public Button removeRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    this.eventManager.removeEventListener(RightMouseDownEvent.class, listener);
    return this;
  }

  /**
   * Accessor for whether or not the button is disabled.
   *
   * @return Boolean value
   */
  public Boolean isDisableOnClick() {
    if (this.control != null) {
      try {
        this.bbjButton.getDisableOnClick();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.disableOnClick;
  }

  /**
   * Mutator for whether or not the button is disabled on click.
   *
   * @param disable Boolean value
   * @return Instance of the object to enable method chaining.
   */
  public Button setDisableOnClick(Boolean disable) {
    if (this.control != null) {
      try {
        this.bbjButton.setDisableOnClick(disable);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.disableOnClick = disable;
    return this;
  }


  /**
   * Accessor for the vertical alignment of text within the button.
   *
   * @return Enum value of text's vertical alignment
   */
  public TextVerticalAlignment getVerticalAlignment() {
    return this.verticalAlignment;
  }

  /**
   * Mutator for the vertical alignment of text within the button.
   *
   * @param alignment TextVerticalAlignment enum value
   * @return The Button itself
   */
  public Button setVerticalAlignment(TextVerticalAlignment alignment) {
    if (this.control != null) {
      try {
        this.bbjButton.setVerticalAlignment(alignment.alignment);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.verticalAlignment = alignment;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Boolean isTabTraversable() {
    return super.isComponentTabTraversable();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Button setTabTraversable(Boolean traversable) {
    super.setComponentTabTraversable(traversable);
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
  public Button setTextAlignment(Alignment alignment) {
    if (this.control != null) {
      try {
        this.bbjButton.setAlignment(alignment.getValue());
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }
    this.textAlignment = alignment;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Button focus() {
    super.focusComponent();
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Button setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isEnabled() {
    return super.isComponentEnabled();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Button setText(String text) {
    super.setText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Button setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Button setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Button setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Button setProperty(String property, Object value) {
    super.setProperty(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Button setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Button addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public Button removeClassName(String selector) {
    super.removeClassName(selector);
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
    this.eventManager.catchUp();

    if (Boolean.TRUE.equals(this.disableOnClick)) {
      this.setDisableOnClick(this.disableOnClick);
    }

    if (this.textAlignment != Alignment.MIDDLE) {
      this.setTextAlignment(this.textAlignment);
    }

    if (this.verticalAlignment != TextVerticalAlignment.CENTER) {
      this.setVerticalAlignment(verticalAlignment);
    }
  }
}
