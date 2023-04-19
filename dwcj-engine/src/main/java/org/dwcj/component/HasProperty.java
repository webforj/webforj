package org.dwcj.component;

/**
 * The HasProperty interface is used to set and retrieve properties of a component.
 *
 * <p>
 * The properties are key-value pairs that can be used to configure the client component. Note the
 * we always reach the client component to set or retrieve a property.
 * </p>
 */
public interface HasProperty {
  /**
   * Retrieve the value of the given property.
   *
   * @param property the key of the property
   * @return the retrieved value
   */
  public Object getProperty(String property);

  /**
   * Set a property value.
   *
   * @param property the key of the property
   * @param value the value to be set
   * @return the control itself
   */
  public HasProperty setProperty(String property, Object value);
}
