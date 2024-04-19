package com.webforj.data;

import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;

/**
 * A utility class for working with JavaBeans.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public final class BeanUtils {
  private BeanUtils() {
    // Prevent instantiation.
  }

  /**
   * Gets the property descriptor for the specified property of the specified bean class.
   *
   * @param propertyName the name of the property.
   * @param beanClass the class of the bean.
   * @return the property descriptor.
   *
   * @throws IntrospectionException if an exception occurs during introspection.
   * @throws IllegalArgumentException if the property is not found in the bean class.
   */
  public static PropertyDescriptor getPropertyDescriptor(String propertyName, Class<?> beanClass)
      throws IntrospectionException {
    for (PropertyDescriptor pd : Introspector.getBeanInfo(beanClass, Object.class)
        .getPropertyDescriptors()) {
      if (pd.getName().equals(propertyName)) {
        return pd;
      }
    }

    throw new IllegalArgumentException(
        "Property '" + propertyName + "' not found in bean class '" + beanClass.getName()
            + "'. Verify the property name and its declaration in the bean class.");
  }
}
