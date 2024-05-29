package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * This interface defines a contract for components that have a typing mode.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
public interface HasTypingMode<T extends Component> {

  /**
   * Describes the typing mode of the component.
   */
  public enum TypingMode {
    /**
     * The characters are inserted into the component when typed.
     */
    INSERT,
    /**
     * The characters are inserted into the component when typed, but if the cursor is on a
     * character, the character is overwritten.
     */
    OVERWRITE
  }

  /**
   * Gets the typing mode of the component.
   *
   * @return The typing mode of the component.
   */
  public default TypingMode getTypingMode() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasTypingMode) {
      return ((HasTypingMode<?>) component).getTypingMode();
    }

    throw new UnsupportedOperationException("The component does not support typing mode");
  }

  /**
   * Sets the typing mode of the component.
   *
   * @param typingMode The typing mode to set for the component.
   * @return The component itself.
   */
  public default T setTypingMode(TypingMode typingMode) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasTypingMode) {
      ((HasTypingMode<?>) component).setTypingMode(typingMode);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support typing mode");
  }
}
