package org.dwcj.concern.legacy;

/**
 * Interface which facilitates implementation of behavior that allows for access and mutation of
 * HTML attributes on a control.
 *
 * @deprecated Use {@link HasAttribute} instead.
 */
@Deprecated(since = "23.05", forRemoval = true)
public interface LegacyHasAttribute {

  /**
   * retrieve the value of the given attribute.
   *
   * @param attribute the key/name of the attribute
   * @return the value
   */
  public String getAttribute(String attribute);

  /**
   * set an attribute value.
   *
   * @param attribute the key/name of the attribute
   * @param value the value
   * @return the control itself
   */
  public LegacyHasAttribute setAttribute(String attribute, String value);

  /**
   * removes an attribute.
   *
   * @param attribute the key/name of the attribute
   * @return the control itself
   */
  public LegacyHasAttribute removeAttribute(String attribute);
}
