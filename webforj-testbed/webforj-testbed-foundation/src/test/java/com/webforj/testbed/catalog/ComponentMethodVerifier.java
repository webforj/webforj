package com.webforj.testbed.catalog;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import com.webforj.component.Component;
import com.webforj.dispatcher.EventListener;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Proxy;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.Month;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TimeZone;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

/**
 * Verifies that every public method of a component is testable under the test context.
 *
 * <p>
 * The verifier invokes every public method the component declares, walking the whole class
 * hierarchy below {@code Component}, with fabricated arguments. A method counts as testable when it
 * either completes or throws from its own code, for example rejecting a null argument. A method
 * fails the verification when the failure originates in the mock layer, which would mean the
 * framework blocks the call.
 * </p>
 */
final class ComponentMethodVerifier {
  private static final String FABRICATED_TEXT = "value";
  private static final Map<Class<?>, Supplier<Object>> FABRICATED_VALUES = createFabricatedValues();

  private ComponentMethodVerifier() {}

  /**
   * Invokes every public method of the given attached component and fails the test on any method
   * the framework cannot carry.
   *
   * @param component the attached component under test
   */
  static void verifyEveryPublicMethod(Component component) {
    verify(component, methodsOf(component.getClass()));
  }

  /**
   * Invokes every public zero argument method of the given attached component and fails the test on
   * any method the framework cannot carry.
   *
   * @param component the attached component
   */
  static void verifyEveryPublicReader(Component component) {
    verify(component, methodsOf(component.getClass()).stream()
        .filter(method -> method.getParameterCount() == 0).toList());
  }

  private static void verify(Component component, List<Method> methods) {
    List<String> gaps = new ArrayList<>();
    List<String> skipped = new ArrayList<>();
    int invoked = 0;

    for (Method method : methods) {
      Optional<Object[]> arguments = fabricateArguments(method);
      if (arguments.isEmpty()) {
        skipped.add(signatureOf(method) + " (argument not fabricated)");
      } else if (invoke(component, method, arguments.get(), gaps, skipped)) {
        invoked++;
      }
    }

    if (!gaps.isEmpty()) {
      fail("The framework blocks " + gaps.size() + " methods of "
          + component.getClass().getSimpleName() + ":" + System.lineSeparator()
          + String.join(System.lineSeparator(), gaps));
    }

    if (!skipped.isEmpty()) {
      fail("The verifier could not invoke " + skipped.size() + " methods of "
          + component.getClass().getSimpleName() + ":" + System.lineSeparator()
          + String.join(System.lineSeparator(), skipped));
    }

    assertTrue(invoked > 0,
        "Expected to invoke the public methods of " + component.getClass().getSimpleName());
  }

  private static Optional<Object[]> fabricateArguments(Method method) {
    Class<?>[] parameterTypes = method.getParameterTypes();
    Object[] arguments = new Object[parameterTypes.length];
    for (int i = 0; i < arguments.length; i++) {
      Optional<Object> argument = fabricate(parameterTypes[i]);
      if (argument.isEmpty() && parameterTypes[i].isPrimitive()) {
        return Optional.empty();
      }
      arguments[i] = argument.orElse(null);
    }

    return Optional.of(arguments);
  }

  private static boolean invoke(Component component, Method method, Object[] arguments,
      List<String> gaps, List<String> skipped) {
    try {
      method.setAccessible(true); // NOSONAR
      method.invoke(component, arguments);

      return true;
    } catch (IllegalAccessException e) {
      skipped.add(signatureOf(method) + " (not accessible)");

      return false;
    } catch (InvocationTargetException e) {
      Throwable cause = e.getCause() == null ? e : e.getCause();
      if (isMockGap(cause)) {
        gaps.add(signatureOf(method) + ": " + cause.getClass().getSimpleName()
            + (cause.getMessage() == null ? "" : " " + cause.getMessage()));

        return false;
      }

      return true;
    }
  }

  /**
   * Returns every public instance method of the component, inherited ones included, deduplicated by
   * signature.
   *
   * <p>
   * The list covers methods declared on the component class, methods inherited from every
   * superclass down to {@code Component}, and default methods inherited from interfaces. Methods
   * declared by {@code Object} are excluded.
   * </p>
   */
  private static List<Method> methodsOf(Class<?> componentClass) {
    Map<String, Method> methods = new LinkedHashMap<>();
    for (Method method : componentClass.getMethods()) {
      if (method.getDeclaringClass() == Object.class || Modifier.isStatic(method.getModifiers())
          || method.isSynthetic()) {
        continue;
      }

      methods.putIfAbsent(signatureOf(method), method);
    }

    List<Method> ordered = new ArrayList<>(methods.values());
    ordered.sort((left, right) -> Boolean.compare(left.getName().equals("destroy"),
        right.getName().equals("destroy")));

    return ordered;
  }

  private static String signatureOf(Method method) {
    StringBuilder signature = new StringBuilder(method.getName()).append('(');
    Class<?>[] parameterTypes = method.getParameterTypes();
    for (int i = 0; i < parameterTypes.length; i++) {
      if (i > 0) {
        signature.append(", ");
      }
      signature.append(parameterTypes[i].getSimpleName());
    }

    return signature.append(')').toString();
  }

  /**
   * Returns whether the failure originates in the mock layer rather than in the component's own
   * contract.
   */
  private static boolean isMockGap(Throwable cause) {
    if (cause instanceof UnsupportedOperationException) {
      return true;
    }

    if (cause instanceof ClassCastException && cause.getMessage() != null
        && cause.getMessage().contains("jdk.proxy")) {
      return true;
    }

    for (StackTraceElement frame : cause.getStackTrace()) {
      if (frame.getClassName().startsWith("com.webforj.testbed.runtime")) {
        return true;
      }
    }

    return false;
  }

  /**
   * Fabricates a deterministic argument for the given parameter type.
   */
  private static Optional<Object> fabricate(Class<?> type) {
    Supplier<Object> fixed = FABRICATED_VALUES.get(type);
    if (fixed != null) {
      return Optional.of(fixed.get());
    }

    if (type.isEnum()) {
      Object[] constants = type.getEnumConstants();

      return constants.length > 0 ? Optional.of(constants[0]) : Optional.empty();
    }

    if (isFunctional(type)) {
      return Optional.of(noOpProxy(type));
    }

    if (type.isArray()) {
      return Optional.of(Array.newInstance(type.getComponentType(), 0));
    }

    if (type.isInterface()) {
      return Optional.of(noOpProxy(type));
    }

    try {
      return Optional.of(type.getConstructor().newInstance());
    } catch (ReflectiveOperationException | RuntimeException e) {
      return Optional.empty();
    }
  }

  private static boolean isFunctional(Class<?> type) {
    return type == EventListener.class || type == Consumer.class || type == Supplier.class
        || type == Function.class || type == Predicate.class
        || (type.isInterface() && type.isAnnotationPresent(FunctionalInterface.class));
  }

  private static Map<Class<?>, Supplier<Object>> createFabricatedValues() {
    Map<Class<?>, Supplier<Object>> values = new HashMap<>();
    values.put(boolean.class, () -> true);
    values.put(Boolean.class, () -> true);
    values.put(int.class, () -> 0);
    values.put(Integer.class, () -> 0);
    values.put(long.class, () -> 0L);
    values.put(Long.class, () -> 0L);
    values.put(double.class, () -> 0d);
    values.put(Double.class, () -> 0d);
    values.put(float.class, () -> 0f);
    values.put(Float.class, () -> 0f);
    values.put(short.class, () -> (short) 0);
    values.put(Short.class, () -> (short) 0);
    values.put(byte.class, () -> (byte) 0);
    values.put(Byte.class, () -> (byte) 0);
    values.put(char.class, () -> 'a');
    values.put(Character.class, () -> 'a');
    values.put(String.class, () -> FABRICATED_TEXT);
    values.put(CharSequence.class, () -> FABRICATED_TEXT);
    values.put(Object.class, () -> FABRICATED_TEXT);
    values.put(List.class, ArrayList::new);
    values.put(Collection.class, ArrayList::new);
    values.put(Set.class, LinkedHashSet::new);
    values.put(Map.class, HashMap::new);
    values.put(Locale.class, () -> Locale.US);
    values.put(TimeZone.class, () -> TimeZone.getTimeZone("UTC"));
    values.put(LocalDate.class, () -> LocalDate.of(2024, Month.JUNE, 15));
    values.put(LocalTime.class, () -> LocalTime.of(10, 30));
    values.put(LocalDateTime.class, () -> LocalDateTime.of(2024, Month.JUNE, 15, 10, 30));

    return values;
  }

  private static Object noOpProxy(Class<?> type) {
    return Proxy.newProxyInstance(ComponentMethodVerifier.class.getClassLoader(),
        new Class<?>[] {type}, (proxy, method, args) -> switch (method.getName()) {
          case "equals" -> proxy == args[0];
          case "hashCode" -> System.identityHashCode(proxy);
          case "toString" -> type.getSimpleName() + " fabricated argument";
          default -> null;
        });
  }
}
