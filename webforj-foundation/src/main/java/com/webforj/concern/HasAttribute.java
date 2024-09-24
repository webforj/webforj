package com.webforj.concern;

import static com.webforj.App.console;
import static com.webforj.App.consoleLog;
import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface that enables components to access and modify HTML DOM attributes.
 *
 * <p>
 * This interface provides methods for retrieving, setting, and removing HTML DOM attributes on a
 * component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasAttribute<T extends Component> {

  /**
   * Retrieves the value of a specified attribute.
   *
   * @param attribute the name of the attribute to retrieve
   * @return the value of the attribute
   */
  public default String getAttribute(String attribute) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasAttribute) {
      return ((HasAttribute<?>) component).getAttribute(attribute);
    }

    throw new UnsupportedOperationException("The component does not support attributes");
  }

  /**
   * Sets the value of an attribute.
   *
   * @param attribute the name of the attribute to set
   * @param value the value to set
   *
   * @return the component itself
   */
  public default T setAttribute(String attribute, String value) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasAttribute) {
      ((HasAttribute<?>) component).setAttribute(attribute, value);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support attributes");
  }

  /**
   * Removes an attribute from the component.
   *
   * @param attribute the name of the attribute to remove
   * @return the component itself
   */
  public default T removeAttribute(String attribute) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasAttribute) {
      ((HasAttribute<?>) component).removeAttribute(attribute);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support attributes");
  }
}
