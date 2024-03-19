package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface that allows components to set and retrieve text in a way that makes sense for the
 * specific component.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasText<T extends Component> {

  /**
   * Retrieves the text property of the component.
   *
   * @return the text of the component.
   */
  public default String getText() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasText) {
      return ((HasText<?>) component).getText();
    }

    throw new UnsupportedOperationException("The component does not support text");
  }

  /**
   * Sets the text of the component. Each component implementing this interface has a text property,
   * which may be visible in different ways (caption, title, contents of edit) or sometimes not
   * visible at all.
   *
   * @param text the text to set for the component.
   * @return the component itself after configuring the text.
   */
  public default T setText(String text) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasText) {
      ((HasText<?>) component).setText(text);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support text");
  }
}
