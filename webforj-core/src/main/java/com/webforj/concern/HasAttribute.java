package com.webforj.concern;

import com.webforj.component.Component;

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
  public String getAttribute(String attribute);

  /**
   * Sets the value of an attribute.
   *
   * @param attribute the name of the attribute to set
   * @param value the value to set
   *
   * @return the component itself
   */
  public T setAttribute(String attribute, String value);

  /**
   * Removes an attribute from the component.
   *
   * @param attribute the name of the attribute to remove
   * @return the component itself
   */
  public T removeAttribute(String attribute);
}
