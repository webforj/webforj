package org.dwcj.concern;

import org.dwcj.component.Component;

/**
 * The HasProperty interface is used to set and retrieve properties of a component.
 *
 * <p>
 * The properties are key-value pairs that can be used to configure the client component. Note the
 * we always reach the client component to set or retrieve a property.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasProperty<T extends Component> {

  /**
   * Retrieves the value of the given property.
   *
   * @param property the key of the property
   * @return the retrieved value.
   */
  public Object getProperty(String property);

  /**
   * Sets a property value.
   *
   * @param property the key of the property
   * @param value the value to be set
   *
   * @return the component itself.
   */
  public T setProperty(String property, Object value);
}
