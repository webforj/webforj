package com.webforj.component.element;

import com.webforj.component.element.annotation.PropertyExclude;
import com.webforj.component.element.annotation.PropertyMethods;
import com.webforj.exceptions.WebforjRuntimeException;
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
   * @throws WebforjRuntimeException If any error occurs during scanning or if a setter method for a
   *         property is not found.
   */
  public static <T, V> List<PropertyDescriptorInfo<V>> scan(Class<T> clazz, T instance,
      Predicate<PropertyDescriptor<V>> descriptorFilter) {
    List<PropertyDescriptorInfo<V>> properties = new ArrayList<>();

    for (Field field : clazz.getDeclaredFields()) {
      field.setAccessible(true); // NOSONAR

      if (field.isAnnotationPresent(PropertyExclude.class)) {
        continue;
      }

      try {
        if (PropertyDescriptor.class.isAssignableFrom(field.getType())) {
          @SuppressWarnings("unchecked")
          PropertyDescriptor<V> descriptor = (PropertyDescriptor<V>) field.get(instance);
          if (descriptorFilter.test(descriptor)) {
            Method getter = findGetter(clazz, field, descriptor);
            Method setter = findSetter(clazz, field, descriptor);
            Class<?> targetClass = getTargetClass(field);
            properties.add(new PropertyDescriptorInfo<>(descriptor, getter, setter, targetClass));
          }
        }
      } catch (IllegalArgumentException | IllegalAccessException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return properties;
  }

  private static <T, V> Method findSetter(Class<T> clazz, Field field,
      PropertyDescriptor<V> descriptor) {
    PropertyMethods methodsAnnotation = field.getAnnotation(PropertyMethods.class);
    String methodName;
    Class<?> targetClass = clazz;

    if (methodsAnnotation != null && !methodsAnnotation.setter().isEmpty()) {
      methodName = methodsAnnotation.setter();
      if (methodsAnnotation.target() != void.class) {
        targetClass = methodsAnnotation.target();
      }
    } else {
      String propName = descriptor.getName();
      methodName = "set" + capitalize(propName);
    }

    Class<V> propType = descriptor.getType();
    Method setter = findMethod(targetClass, methodName, getPrimitiveType(propType));

    if (setter == null) {
      setter = findMethod(targetClass, methodName, propType);
    }

    if (setter == null) {
      // Additional logic for handling complex types like List<Object>
      setter = findCompatibleSetter(targetClass, methodName, descriptor);
    }

    if (setter == null) {
      throw new WebforjRuntimeException("Setter method for " + descriptor.getName()
          + " is not found in class " + targetClass.getName());
    }

    return setter;
  }

  private static <T, V> Method findGetter(Class<T> clazz, Field field,
      PropertyDescriptor<V> descriptor) {
    PropertyMethods methodsAnnotation = field.getAnnotation(PropertyMethods.class);
    String methodName;
    Class<?> targetClass = clazz;

    if (methodsAnnotation != null && !methodsAnnotation.getter().isEmpty()) {
      methodName = methodsAnnotation.getter();
      if (methodsAnnotation.target() != void.class) {
        targetClass = methodsAnnotation.target();
      }
    } else {
      String propName = descriptor.getName();
      Class<V> propType = descriptor.getType();
      if (propType.equals(boolean.class) || propType.equals(Boolean.class)) {
        String isGetterName = "is" + capitalize(propName);
        String hasGetterName = "has" + capitalize(propName);

        Method getter = findMethod(targetClass, isGetterName);
        if (getter == null) {
          getter = findMethod(targetClass, hasGetterName);
        }
        if (getter != null) {
          return getter;
        }
      }
      methodName = "get" + capitalize(propName);
    }

    Method getter = findMethod(targetClass, methodName);

    if (getter == null) {
      // Additional logic for handling complex types like List<Object>
      getter = findCompatibleGetter(targetClass, descriptor.getName(), descriptor);
    }

    if (getter == null) {
      throw new WebforjRuntimeException("Getter method for property '" + descriptor.getName()
          + "' not found in class " + targetClass.getName());
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
      // such as Object, List, Collection, etc., for complex types.
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

  private static Class<?> getTargetClass(Field field) {
    PropertyMethods methodsAnnotation = field.getAnnotation(PropertyMethods.class);
    if (methodsAnnotation != null && methodsAnnotation.target() != void.class) {
      return methodsAnnotation.target();
    }

    return field.getDeclaringClass();
  }
}
