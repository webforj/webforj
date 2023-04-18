package org.dwcj.component;

/**
 * Interface which facilitates implementation of behavior that allows for properties on a control.
 */
public interface HasProperty {

  /**
   * retrieve the value of the given property.
   *
   * @param property the key of the attribute
   * @return the retrieved value
   */
  public String getProperty(String property);

  /**
   * set a property value.
   *
   * @param property the key of the property
   * @param value the value to be set
   * @return the control itself
   */
  public HasProperty setProperty(String property, String value);

}
