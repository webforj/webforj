package com.webforj.testbed.runtime;

import com.basis.startup.type.BBjVector;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The base for all runtime mocks in the webforJ unit testing framework.
 *
 * <p>
 * A runtime mock is a stateful test double for one of the runtime interfaces that back webforJ
 * components. Each mock exposes its behavior through a dynamic proxy created with
 * {@link #getProxy(Class)}. The proxy routes every invocation through the mock so that state
 * written by one method is visible to its counterpart, for example a value passed to a setter is
 * returned by the matching getter.
 * </p>
 *
 * <p>
 * Invocations resolve in the following order.
 * </p>
 *
 * <ol>
 * <li>{@code equals}, {@code hashCode} and {@code toString} use proxy identity semantics.</li>
 * <li>{@link #handle(Object, Method, Object[])} gives subclasses the first chance to implement
 * behavior with real state.</li>
 * <li>Symmetric accessor pairs are resolved automatically. A single argument method named
 * {@code setX} stores the value under the key {@code x} and a zero argument method named
 * {@code getX} or {@code isX} returns it.</li>
 * <li>Any remaining invocation whose return type is another runtime interface returns a cached
 * generic mock, and every other invocation returns a neutral default value for its return type, so
 * that unforeseen calls never fail.</li>
 * </ol>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public abstract class RuntimeMock {
  /**
   * The sentinel returned by {@link #handle(Object, Method, Object[])} when the subclass does not
   * implement the invoked method.
   */
  protected static final Object UNHANDLED = new Object();

  private static final Map<Class<?>, Object> ZERO_VALUES = createZeroValues();

  private final Map<String, Object> values = new ConcurrentHashMap<>();
  private final Map<Class<?>, Object> proxies = new ConcurrentHashMap<>();
  private final Map<Class<?>, Object> genericChildren = new ConcurrentHashMap<>();

  /**
   * Returns a proxy implementing the given interface and backed by this mock.
   *
   * <p>
   * The proxy is created once per interface and cached, so repeated calls with the same interface
   * return the same instance.
   * </p>
   *
   * @param <T> the interface type
   * @param type the interface to implement
   *
   * @return the proxy
   */
  public <T> T getProxy(Class<T> type) {
    Object cached =
        proxies.computeIfAbsent(type, key -> Proxy.newProxyInstance(getClass().getClassLoader(),
            new Class<?>[] {key}, new RuntimeMockInvocationHandler()));

    return type.cast(cached);
  }

  /**
   * Implements behavior for the invoked method.
   *
   * <p>
   * Subclasses override this method and return a result for every method they implement with real
   * state. Returning {@link #UNHANDLED} delegates the invocation to the symmetric accessor store
   * and finally to the neutral default value for the return type.
   * </p>
   *
   * @param proxy the proxy the method was invoked on
   * @param method the invoked method
   * @param args the invocation arguments, never null
   *
   * @return the result, or {@link #UNHANDLED} when not implemented
   * @throws Exception if the behavior fails
   */
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception { // NOSONAR
    return UNHANDLED;
  }

  /**
   * Stores a value in the symmetric accessor store.
   *
   * @param key the store key
   * @param value the value to store, null removes the entry
   */
  protected final void setValue(String key, Object value) {
    if (value == null) {
      values.remove(key);
    } else {
      values.put(key, value);
    }
  }

  /**
   * Returns a value from the symmetric accessor store.
   *
   * @param key the store key
   *
   * @return the stored value, or null when absent
   */
  protected final Object getValue(String key) {
    return values.get(key);
  }

  /**
   * Returns a value from the symmetric accessor store, falling back to the given default.
   *
   * @param <T> the value type
   * @param key the store key
   * @param defaultValue the value to return when the key is absent
   *
   * @return the stored value, or the default when absent
   */
  @SuppressWarnings("unchecked")
  protected final <T> T getValue(String key, T defaultValue) {
    Object value = values.get(key);

    return value == null ? defaultValue : (T) value;
  }

  /**
   * Returns the mock backing the given proxy.
   *
   * @param proxy the proxy to resolve
   *
   * @return the backing mock, or an empty optional when the object is not backed by a mock
   */
  public static Optional<RuntimeMock> getBacking(Object proxy) {
    if (proxy != null && Proxy.isProxyClass(proxy.getClass())
        && Proxy.getInvocationHandler(proxy) instanceof RuntimeMockInvocationHandler handler) {
      return Optional.of(handler.owner());
    }

    return Optional.empty();
  }

  /**
   * Returns the neutral default value for the given type.
   *
   * <p>
   * Primitives return their zero value, {@code String} returns the empty string, {@link BBjVector}
   * returns an empty vector and every other type returns null.
   * </p>
   *
   * @param type the return type
   *
   * @return the neutral default value
   */
  protected static Object defaultValue(Class<?> type) {
    Object zero = ZERO_VALUES.get(type);
    if (zero != null) {
      return zero;
    }

    if (type == String.class) {
      return "";
    }

    if (type == BBjVector.class) {
      return new BBjVector();
    }

    return null;
  }

  // Keyed by the primitive types and their wrappers, each mapped to its zero value.
  private static Map<Class<?>, Object> createZeroValues() {
    Map<Class<?>, Object> zeros = new HashMap<>();
    zeros.put(boolean.class, false);
    zeros.put(Boolean.class, false);
    zeros.put(int.class, 0);
    zeros.put(Integer.class, 0);
    zeros.put(long.class, 0L);
    zeros.put(Long.class, 0L);
    zeros.put(double.class, 0d);
    zeros.put(Double.class, 0d);
    zeros.put(float.class, 0f);
    zeros.put(Float.class, 0f);
    zeros.put(short.class, (short) 0);
    zeros.put(Short.class, (short) 0);
    zeros.put(byte.class, (byte) 0);
    zeros.put(Byte.class, (byte) 0);
    zeros.put(char.class, (char) 0);
    zeros.put(Character.class, (char) 0);

    return Collections.unmodifiableMap(zeros);
  }

  /**
   * Derives the symmetric store key for an accessor method name.
   *
   * @param methodName the method name
   * @param prefixLength the length of the accessor prefix
   *
   * @return the store key
   */
  private static String propertyKey(String methodName, int prefixLength) {
    String name = methodName.substring(prefixLength);
    if (name.isEmpty()) {
      return name;
    }

    return Character.toLowerCase(name.charAt(0)) + name.substring(1);
  }

  private final class RuntimeMockInvocationHandler implements InvocationHandler {
    RuntimeMock owner() {
      return RuntimeMock.this;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
      Object[] arguments = args == null ? new Object[0] : args;
      Object result = invokeObjectMethod(proxy, method.getName(), arguments);
      if (result == UNHANDLED) {
        try {
          result = handle(proxy, method, arguments);
        } catch (InvocationTargetException e) {
          throw e.getCause() == null ? e : e.getCause();
        }
      }

      if (result == UNHANDLED) {
        result = invokeAccessor(method, arguments);
      }

      if (result == UNHANDLED) {
        result = fallbackOf(method.getReturnType());
      }

      return result;
    }

    private Object invokeObjectMethod(Object proxy, String name, Object[] arguments) {
      if (name.equals("equals") && arguments.length == 1) {
        return proxy == arguments[0];
      }

      if (name.equals("hashCode") && arguments.length == 0) {
        return System.identityHashCode(RuntimeMock.this);
      }

      if (name.equals("toString") && arguments.length == 0) {
        return RuntimeMock.this.getClass().getSimpleName() + "@"
            + Integer.toHexString(System.identityHashCode(RuntimeMock.this));
      }

      return UNHANDLED;
    }

    private Object invokeAccessor(Method method, Object[] arguments) {
      String name = method.getName();
      if (arguments.length == 1 && name.startsWith("set") && name.length() > 3) {
        setValue(propertyKey(name, 3), arguments[0]);

        return defaultValue(method.getReturnType());
      }

      if (arguments.length == 0 && name.startsWith("get") && name.length() > 3) {
        Object stored = getValue(propertyKey(name, 3));
        if (stored != null) {
          return stored;
        }
      }

      if (arguments.length == 0 && name.startsWith("is") && name.length() > 2) {
        Object stored = getValue(propertyKey(name, 2));
        if (stored != null) {
          return stored;
        }
      }

      return UNHANDLED;
    }

    private Object fallbackOf(Class<?> returnType) {
      if (returnType.isInterface() && returnType.getName().startsWith("com.basis.")) {
        return genericChildren.computeIfAbsent(returnType,
            type -> new RuntimeMock() {}.getProxy(type));
      }

      return defaultValue(returnType);
    }
  }

  /**
   * A registered runtime callback consisting of a handler object and the name of the method to
   * invoke on it.
   *
   * @param handler the object carrying the callback method
   * @param methodName the name of the callback method
   *
   * @since 26.03
   */
  public static final class Callback {
    private final Object handler;
    private final String methodName;

    public Callback(Object handler, String methodName) {
      this.handler = handler;
      this.methodName = methodName;
    }

    /**
     * Invokes the callback with the given event object.
     *
     * <p>
     * The callback method is resolved by name on the handler with a single parameter that is
     * assignable from the event type.
     * </p>
     *
     * @param event the event to pass to the callback
     *
     * @throws IllegalStateException if the handler has no matching method or the invocation fails
     */
    public void invoke(Object event) {
      Method target = null;
      for (Method candidate : handler.getClass().getMethods()) {
        if (candidate.getName().equals(methodName) && candidate.getParameterCount() == 1
            && candidate.getParameterTypes()[0].isInstance(event)) {
          target = candidate;
          break;
        }
      }

      if (target == null) {
        throw new IllegalStateException("No callback method named '" + methodName + "' accepting "
            + event.getClass().getName() + " found on " + handler.getClass().getName());
      }

      try {
        target.setAccessible(true); // NOSONAR
        target.invoke(handler, event);
      } catch (IllegalAccessException e) {
        throw new IllegalStateException("Failed to access callback method '" + methodName + "' on "
            + handler.getClass().getName(), e);
      } catch (InvocationTargetException e) {
        Throwable cause = e.getCause();
        if (cause instanceof RuntimeException runtimeException) {
          throw runtimeException;
        }

        if (cause instanceof Error error) {
          throw error;
        }

        throw new IllegalStateException(
            "Callback method '" + methodName + "' on " + handler.getClass().getName() + " failed",
            e);
      }
    }
  }

  /**
   * Creates a proxy for any runtime event interface backed by a map of return values.
   *
   * <p>
   * The proxy returns the map entry whose key matches the invoked method name. Methods without an
   * entry return the neutral default value for their return type.
   * </p>
   *
   * @param <E> the event interface type
   * @param type the event interface to implement
   * @param returnValues the return values keyed by method name
   *
   * @return the event proxy
   */
  public static <E> E createEvent(Class<E> type, Map<String, Object> returnValues) {
    Map<String, Object> copy = new HashMap<>(returnValues);

    return type.cast(Proxy.newProxyInstance(RuntimeMock.class.getClassLoader(),
        new Class<?>[] {type}, (proxy, method, args) -> {
          String name = method.getName();
          if (name.equals("toString") && (args == null || args.length == 0)) {
            return type.getSimpleName() + copy;
          }

          if (name.equals("hashCode") && (args == null || args.length == 0)) {
            return System.identityHashCode(proxy);
          }

          if (name.equals("equals") && args != null && args.length == 1) {
            return proxy == args[0];
          }

          if (copy.containsKey(name)) {
            return copy.get(name);
          }

          return defaultValue(method.getReturnType());
        }));
  }
}
