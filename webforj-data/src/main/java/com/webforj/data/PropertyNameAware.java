package com.webforj.data;

/**
 * Interface for classes that are aware of a bean property name.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public interface PropertyNameAware {

  /**
   * Sets the property name.
   *
   * @param propertyName the property name.
   */
  public void setPropertyName(String propertyName);

  /**
   * Gets the property name.
   *
   * @return the property name.
   */
  public String getPropertyName();
}
