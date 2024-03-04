package com.webforj.component.element;

import com.webforj.exceptions.DwcjRuntimeException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;

/**
 * A utility class for scanning a class for property descriptors and their associated getter and
 * setter methods.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
final class PropertyDescriptorScanner {

  private PropertyDescriptorScanner() {}

  /**
   * Scans the specified class for property descriptors and their associated getter and setter
   * methods.
   *
   * @param <T> The type of the class to scan.
   * @param <V> The type of the property value.
   * @param clazz The class to scan for property descriptors.
   * @param instance An instance of the class, which is used to access the property descriptors.
   * @param descriptorFilter A filter predicate to include only selected property descriptors.
   *
   * @return A list of {@link PropertyDescriptorInfo} instances representing the selected property
   *         descriptors and their associated getter and setter methods.
   * @throws DwcjRuntimeException If any error occurs during scanning or if a setter method for a
   *         property is not found.
   */
  public static <T, V> List<PropertyDescriptorInfo<V>> scan(Class<T> clazz, T instance,
      Predicate<PropertyDescriptor<V>> descriptorFilter) {
    List<PropertyDescriptorInfo<V>> properties = new ArrayList<>();

    for (Field field : clazz.getDeclaredFields()) {
      field.setAccessible(true); // NOSONAR
      try {
        if (PropertyDescriptor.class.isAssignableFrom(field.getType())) {
          @SuppressWarnings("unchecked")
          PropertyDescriptor<V> descriptor = (PropertyDescriptor<V>) field.get(instance);
          if (descriptorFilter.test(descriptor)) {
            Method getter = findGetter(clazz, descriptor);
            Method setter = findSetter(clazz, descriptor);
            properties.add(new PropertyDescriptorInfo<>(descriptor, getter, setter));
          }
        }
      } catch (IllegalArgumentException | IllegalAccessException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    return properties;
  }

  private static <T, V> Method findSetter(Class<T> clazz, PropertyDescriptor<V> descriptor) {
    String propName = descriptor.getName();
    Class<V> propType = descriptor.getType();
    String methodName = "set" + capitalize(propName);
    Method setter = findMethod(clazz, methodName, getPrimitiveType(propType));

    if (setter == null) {
      setter = findMethod(clazz, methodName, propType);
    }

    if (setter == null) {
      // Additional logic for handling complex types like List<Object>
      setter = findCompatibleSetter(clazz, methodName, descriptor);
    }

    if (setter == null) {
      throw new DwcjRuntimeException(
          "Setter method for " + propName + " is not found in class " + clazz.getName());
    }

    return setter;
  }

  private static <T, V> Method findGetter(Class<T> clazz, PropertyDescriptor<V> descriptor) {
    String propName = descriptor.getName();
    Class<V> propType = descriptor.getType();
    Method getter = null;

    if (propType.equals(boolean.class) || propType.equals(Boolean.class)) {
      String isGetterName = "is" + capitalize(propName);
      String hasGetterName = "has" + capitalize(propName);

      getter = findMethod(clazz, isGetterName);
      if (getter == null) {
        getter = findMethod(clazz, hasGetterName);
      }
    }

    if (getter == null) {
      String getGetterName = "get" + capitalize(propName);
      getter = findMethod(clazz, getGetterName);
    }

    if (getter == null) {
      // Additional logic for handling complex types like List<Object>
      getter = findCompatibleGetter(clazz, propName, descriptor);
    }

    if (getter == null) {
      throw new DwcjRuntimeException(
          "Getter method for property '" + propName + "' not found in class " + clazz.getName());
    }

    return getter;
  }

  private static <T, V> Method findCompatibleSetter(Class<T> clazz, String methodName,
      PropertyDescriptor<V> descriptor) {
    // Iterate over all methods and find a compatible setter
    for (Method method : clazz.getMethods()) {
      if (method.getName().equals(methodName) && method.getParameterTypes().length == 1
          && method.getParameterTypes()[0].isAssignableFrom(descriptor.getType())) {
        return method;
      }
    }
    return null;
  }

  private static <T, V> Method findCompatibleGetter(Class<T> clazz, String propName,
      PropertyDescriptor<V> descriptor) {
    // Iterate over all methods and find a compatible getter
    for (Method method : clazz.getMethods()) {
      if ((method.getName().equals("get" + capitalize(propName))
          || method.getName().equals("is" + capitalize(propName)))
          && method.getReturnType().isAssignableFrom(descriptor.getType())
          && method.getParameterTypes().length == 0) {
        return method;
      }
    }
    return null;
  }


  private static Method findMethod(Class<?> clazz, String methodName, Class<?>... paramTypes) {
    // First, try to find a method with an exact match.
    try {
      return clazz.getMethod(methodName, paramTypes);
    } catch (NoSuchMethodException e) {
      // If not found, look for a compatible method.
      for (Method method : clazz.getMethods()) {
        if (isMethodCompatible(method, methodName, paramTypes)) {
          return method;
        }
      }
    }
    return null;
  }

  private static boolean isMethodCompatible(Method method, String methodName,
      Class<?>... paramTypes) {
    if (!method.getName().equals(methodName)) {
      return false;
    }

    Class<?>[] methodParamTypes = method.getParameterTypes();

    if (paramTypes.length != methodParamTypes.length) {
      return false;
    }

    for (int i = 0; i < paramTypes.length; i++) {
      // Check if each parameter is assignable, allowing for broader compatibility
      // such as Object, List, Collection, etc., for complex types.0
      if (!methodParamTypes[i].isAssignableFrom(paramTypes[i])) {
        return false;
      }
    }

    return true;
  }

  private static Class<?> getPrimitiveType(Class<?> type) {
    if (type == Boolean.class) {
      return boolean.class;
    }
    if (type == Byte.class) {
      return byte.class;
    }
    if (type == Character.class) {
      return char.class;
    }
    if (type == Double.class) {
      return double.class;
    }
    if (type == Float.class) {
      return float.class;
    }
    if (type == Integer.class) {
      return int.class;
    }
    if (type == Long.class) {
      return long.class;
    }
    if (type == Short.class) {
      return short.class;
    }

    // Return the same class if it's not a wrapper type
    return type;
  }

  private static String capitalize(String name) {
    if (name == null || name.isEmpty()) {
      return name;
    }
    return name.substring(0, 1).toUpperCase() + name.substring(1);
  }
}
