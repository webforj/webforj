package org.dwcj.component.element;

/**
 * The PropertyDescriptor class is used to describe a property or an attribute of an element.
 *
 * @param <T> the type of the property
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public final class PropertyDescriptor<T> {
  private final String name;
  private final T defaultValue;
  private final boolean isAttribute;

  /**
   * Create a new property descriptor.
   *
   * @param name the name of the property
   * @param defaultValue the default value of the property
   * @param isAttribute true if the property is an attribute
   */
  public PropertyDescriptor(String name, T defaultValue, boolean isAttribute) {
    this.name = name;
    this.defaultValue = defaultValue;
    this.isAttribute = isAttribute;
  }

  /**
   * Get the name of the property.
   *
   * @return the name of the property
   */
  public String getName() {
    return name;
  }

  /**
   * Get the default value of the property.
   *
   * @return the default value of the property
   */
  public T getDefaultValue() {
    return defaultValue;
  }

  /**
   * Check if the property descriptor is an attribute.
   *
   * @return true if the property descriptor is an attribute
   */
  public boolean isAttribute() {
    return isAttribute;
  }

  /**
   * Check if the property descriptor is a property.
   *
   * @return true if the property descriptor is a property
   */
  public boolean isProperty() {
    return !isAttribute;
  }

  /**
   * Get the type of the property descriptor.
   *
   * @return the type of the property descriptor
   */
  Class<T> getType() {
    if (defaultValue == null) {
      return null;
    }

    @SuppressWarnings("unchecked")
    Class<T> type = (Class<T>) defaultValue.getClass();
    return type;
  }

  /**
   * Create a new descriptor for a property.
   *
   * @param name the name of the property
   * @param defaultValue the default value of the property
   *
   * @return the property descriptor
   */
  public static <T> PropertyDescriptor<T> property(String name, T defaultValue) {
    return new PropertyDescriptor<>(name, defaultValue, false);
  }

  /**
   * Create a new descriptor for an attribute.
   *
   * @param name the name of the attribute
   * @param defaultValue the default value of the attribute
   *
   * @return the attribute descriptor
   */
  public static <T> PropertyDescriptor<T> attribute(String name, T defaultValue) {
    return new PropertyDescriptor<>(name, defaultValue, true);
  }
}
