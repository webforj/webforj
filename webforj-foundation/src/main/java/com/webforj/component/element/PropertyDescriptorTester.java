package com.webforj.component.element;

import com.webforj.annotation.Experimental;
import com.webforj.exceptions.WebforjRuntimeException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;
import java.util.function.Predicate;

/**
 * A utility class for testing property descriptors and their getter and setter methods.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@Experimental(since = "23.06")
public final class PropertyDescriptorTester {

  private PropertyDescriptorTester() {}

  /**
   * Runs property descriptor tests for the specified class and instance, considering all property
   * descriptors.
   *
   * @param <T> The type of the class to test.
   * @param clazz The class to test property descriptors for.
   * @param instance An instance of the class to test property access.
   */
  public static <T> void run(Class<T> clazz, T instance) {
    run(clazz, instance, field -> true);
  }

  /**
   * Runs property descriptor tests for the specified class and instance, considering only property
   * descriptors that match the given filter predicate.
   *
   * @param <T> The type of the class to test.
   * @param <V> The type of the property value.
   * @param clazz The class to test property descriptors for.
   * @param instance An instance of the class to test property access.
   * @param descriptorFilter A filter predicate to include only selected property descriptors.
   */
  public static <T, V> void run(Class<T> clazz, T instance,
      Predicate<PropertyDescriptor<V>> descriptorFilter) {
    List<PropertyDescriptorInfo<V>> properties =
        PropertyDescriptorScanner.scan(clazz, instance, descriptorFilter);
    for (PropertyDescriptorInfo<V> property : properties) {
      testSingleProp(property, instance);
    }
  }

  private static <T, V> void testSingleProp(PropertyDescriptorInfo<V> info, T instance) {
    PropertyDescriptor<V> descriptor = info.getPropertyDescriptor();
    V defaultValue = descriptor.getDefaultValue();

    try {
      Object targetInstance = getTargetInstance(instance, info.getTargetClass());
      info.getSetter().invoke(targetInstance, defaultValue);

      Method getter = info.getGetter();
      @SuppressWarnings("unchecked")
      V value = (V) getter.invoke(targetInstance);

      if (!isEqual(defaultValue, value)) {
        throw new AssertionError("Property '" + descriptor.getName()
            + "' failed to set/get value. Expected: " + defaultValue + ", Actual: " + value);
      }

    } catch (SecurityException | IllegalAccessException | IllegalArgumentException
        | InvocationTargetException e) {
      throw new WebforjRuntimeException(e);
    }
  }

  private static <T> Object getTargetInstance(T instance, Class<?> targetClass) {
    if (targetClass.isInstance(instance)) {
      return instance;
    }

    for (Class<?> innerClass : instance.getClass().getDeclaredClasses()) {
      if (targetClass.equals(innerClass)) {
        try {
          return innerClass.getDeclaredConstructor(instance.getClass()).newInstance(instance);
        } catch (Exception e) {
          throw new WebforjRuntimeException(e);
        }
      }
    }

    throw new WebforjRuntimeException(
        "Target class instance could not be created for " + targetClass.getName());
  }

  private static <V> boolean isEqual(V a, V b) {
    if (a == null) {
      return b == null;
    }

    return a.equals(b);
  }
}
