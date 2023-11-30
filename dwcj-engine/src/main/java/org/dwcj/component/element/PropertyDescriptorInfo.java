package org.dwcj.component.element;

import java.lang.reflect.Method;

/**
 * A class that encapsulates information about a property descriptor along with its associated read
 * and write methods.
 *
 * @param <V> The type of the property value.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
class PropertyDescriptorInfo<V> {
  private final PropertyDescriptor<V> descriptor;
  private final Method reader;
  private final Method writer;

  /**
   * Constructs a new PropertyDescriptorInfo instance with the provided property descriptor, read
   * method, and write method.
   *
   * @param descriptor The property descriptor associated with this information.
   * @param reader The read method associated with the property descriptor.
   * @param writer The write method associated with the property descriptor.
   */
  PropertyDescriptorInfo(PropertyDescriptor<V> descriptor, Method reader, Method writer) {
    this.descriptor = descriptor;
    this.reader = reader;
    this.writer = writer;
  }

  /**
   * Retrieves the property descriptor associated with this PropertyDescriptorInfo.
   *
   * @return The property descriptor.
   */
  public PropertyDescriptor<V> getPropertyDescriptor() {
    return descriptor;
  }

  /**
   * Retrieves the read method associated with the property descriptor.
   *
   * @return The read method.
   */
  public Method getGetter() {
    return reader;
  }

  /**
   * Retrieves the write method associated with the property descriptor.
   *
   * @return The write method.
   */
  public Method getSetter() {
    return writer;
  }
}
