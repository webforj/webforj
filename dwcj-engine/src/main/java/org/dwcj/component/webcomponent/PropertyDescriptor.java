package org.dwcj.component.webcomponent;

/**
 * The PropertyDescriptor class is used to describe a property or an attribute
 * of a web component.
 * 
 * <pre>
 * {@code @NodeName("my-avatar")
 * public class MyAvatar extends WebComponent {
 *   public static final PropertyDescriptor<String> NAME = PropertyDescriptor.property("name", "Hyyan Abo Fakher");
 *   public static final PropertyDescriptor<Integer> SIZE = PropertyDescriptor.property("size", 100);
 * 
 *   public MyAvatar setName(String name) {
 *     set(NAME, name);
 *     return this;
 *   }
 * 
 *   public String getName() {
 *     return get(NAME);
 *   }
 * 
 *   public MyAvatar setSize(Integer size) {
 *     set(SIZE, size);
 *     return this;
 *   }
 * 
 *   public Integer getSize() {
 *     return get(SIZE);
 *   }
 * }
 * }
 * </pre>
 *
 * @param <T> the type of the property
 * 
 * @see WebComponent
 * @author Hyyan Abo Fakher
 */
public class PropertyDescriptor<T> {
  private final String name;
  private final T defaultValue;
  private final boolean isAttribute;

  /**
   * Create a new property descriptor
   * 
   * @param name         the name of the property
   * @param defaultValue the default value of the property
   * @param isAttribute  true if the property is an attribute
   */
  public PropertyDescriptor(String name, T defaultValue, boolean isAttribute) {
    super();
    this.name = name;
    this.defaultValue = defaultValue;
    this.isAttribute = isAttribute;
  }

  /**
   * Get the name of the property
   * 
   * @return the name of the property
   */
  public String getName() {
    return name;
  }

  /**
   * Get the default value of the property
   * 
   * @return the default value of the property
   */
  public T getDefaultValue() {
    return defaultValue;
  }

  /**
   * Check if the property descriptor is an attribute
   * 
   * @return true if the property descriptor is an attribute
   */
  public boolean isAttribute() {
    return isAttribute;
  }

  /**
   * Check if the property descriptor is a property
   * 
   * @return true if the property descriptor is a property
   */
  public boolean isProperty() {
    return !isAttribute;
  }

  /**
   * Create a new descriptor for a property
   * 
   * @param name         the name of the property
   * @param defaultValue the default value of the property
   * 
   * @return the property descriptor
   */
  public static <T> PropertyDescriptor<T> property(String name, T defaultValue) {
    return new PropertyDescriptor<>(name, defaultValue, false);
  }

  /**
   * Create a new descriptor for an attribute
   * 
   * @param name         the name of the attribute
   * @param defaultValue the default value of the attribute
   * 
   * @return the attribute descriptor
   */
  public static <T> PropertyDescriptor<T> attribute(String name, T defaultValue) {
    return new PropertyDescriptor<>(name, defaultValue, true);
  }
}