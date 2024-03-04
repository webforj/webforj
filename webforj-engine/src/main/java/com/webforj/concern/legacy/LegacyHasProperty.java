package com.webforj.concern.legacy;

/**
 * The HasProperty interface is used to set and retrieve properties of a component.
 *
 * <p>
 * The properties are key-value pairs that can be used to configure the client component. Note the
 * we always reach the client component to set or retrieve a property.
 * </p>
 *
 * @deprecated Use {@link HasProperty} instead.
 */
@Deprecated(since = "23.05", forRemoval = true)
public interface LegacyHasProperty {
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
  public LegacyHasProperty setProperty(String property, Object value);
}
