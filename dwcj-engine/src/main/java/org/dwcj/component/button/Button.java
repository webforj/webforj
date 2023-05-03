package org.dwcj.component.button;

import com.basis.bbj.proxies.sysgui.BBjButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import java.io.File;
import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.HasEnable;
import org.dwcj.component.HasFocus;
import org.dwcj.component.TabTraversable;
import org.dwcj.component.TextAlignable;
import org.dwcj.component.button.event.ButtonClickEvent;
import org.dwcj.component.button.sink.ButtonClickEventSink;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.utilities.BBjFunctionalityHelper;
import org.dwcj.utilities.ImageUtil;


/**
 * A Push Button.
 */
public final class Button extends AbstractDwcComponent
    implements HasFocus, TabTraversable, TextAlignable, HasEnable {

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

  private EventDispatcher dispatcher = new EventDispatcher();
  private ButtonClickEventSink clickEventsSink;
  private Boolean disableOnClick = false;
  private TextVerticalAlignment verticalAlignment = TextVerticalAlignment.CENTER;
  private BBjButton bbjButton;

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
  }

  @Override
  protected void create(AbstractWindow p) {

    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      control = w.addButton(super.getText(), flags);
      this.clickEventsSink = new ButtonClickEventSink(this, dispatcher);
      this.bbjButton = (BBjButton) control;
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public void setImage(String path) {
    this.setImage(new File(path));
  }

  public void setImage(File file) {
    if (this.bbjButton != null) {
      try {
        this.bbjButton.setImage(ImageUtil.convertFileToBBjImage(file));
      } catch (BBjException e) {
        // TODO Auto-generated catch block
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
    if (this.control != null && this.dispatcher.getListenersCount(ButtonClickEvent.class) == 0) {
      this.clickEventsSink.setCallback();
    }
    dispatcher.addEventListener(ButtonClickEvent.class, listener);
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
    dispatcher.removeEventListener(ButtonClickEvent.class, listener);
    if (this.control != null && this.dispatcher.getListenersCount(ButtonClickEvent.class) == 0) {
      this.clickEventsSink.removeCallback();
    }
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
        ((BBjButton) control).getDisableOnClick();
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
        ((BBjButton) control).setDisableOnClick(disable);
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
    if (this.control != null) {
      return this.verticalAlignment;
    }
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
        ((BBjButton) control).setVerticalAlignment(alignment.alignment);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.verticalAlignment = alignment;
    return this;
  }


  /*
   * ===================================================================================== This
   * section overrides the various base class abstract methods in the AbstractDwcjControl class.
   * These need to be should for method chaining purposes (i.e.
   * setExample().setExample2().setExample3() ).
   * =====================================================================================
   */

  @Override
  public Button setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public Button setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public Button setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
  }

  @Override
  public boolean isEnabled() {
    return super.isComponentEnabled();
  }

  @Override
  public Button setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  @Override
  public Button setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public Button setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  @Override
  public Button addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  @Override
  public Button removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }

  @Override
  public Button focus() {
    super.focusComponent();
    return this;
  }


  @Override
  public Boolean isTabTraversable() {
    if (this.control != null) {
      try {
        ((BBjButton) control).isTabTraversable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.tabTraversable;
  }

  @Override
  public Button setTabTraversable(Boolean traversable) {
    if (this.control != null) {
      try {
        ((BBjButton) control).setTabTraversable(traversable);
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
  public Button setTextAlignment(Alignment alignment) {
    if (this.control != null) {
      try {
        ((BBjButton) control).setAlignment(alignment.getValue());
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.textAlignment = alignment;
    return this;
  }


  @Override
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp())) {
      throw new IllegalAccessException("catchUp cannot be called twice");
    }

    super.catchUp();

    if (this.dispatcher.getListenersCount(ButtonClickEvent.class) > 0) {
      this.clickEventsSink.setCallback();
    }

    if (Boolean.TRUE.equals(this.disableOnClick)) {
      this.setDisableOnClick(this.disableOnClick);
    }

    if (Boolean.FALSE.equals(this.tabTraversable)) {
      this.setTabTraversable(this.tabTraversable);
    }

    if (this.textAlignment != Alignment.MIDDLE) {
      this.setTextAlignment(this.textAlignment);
    }
  }
}