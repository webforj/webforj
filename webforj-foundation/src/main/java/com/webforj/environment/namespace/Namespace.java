package com.webforj.environment.namespace;

import com.basis.bbj.proxies.BBjNamespace;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.Environment;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.environment.namespace.event.NamespaceAccessEvent;
import com.webforj.environment.namespace.event.NamespaceChangeEvent;
import com.webforj.environment.namespace.event.NamespaceKeyAccessEvent;
import com.webforj.environment.namespace.event.NamespaceKeyChangeEvent;
import com.webforj.environment.namespace.exception.NamespaceLockedException;
import com.webforj.environment.namespace.sink.NamespaceAccessEventSink;
import com.webforj.environment.namespace.sink.NamespaceChangeEventSink;
import com.webforj.environment.namespace.sink.NamespaceEventSinkRegistry;
import com.webforj.environment.namespace.sink.NamespaceKeyAccessEventSink;
import com.webforj.environment.namespace.sink.NamespaceKeyChangeEventSink;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.function.Function;

/**
 * A Namespace is a shared object space, like a singleton. Depending on its type it's shared
 * globally across all clients, a family of clients started by the same parent, or has a private
 * name.
 *
 * @since 24.22
 * @author Stephen Wald
 * @author Hyyan Abo Fakher
 */
public abstract sealed

class Namespace
permits PrivateNamespace, GlobalNamespace, GroupNamespace
{
  public static final String ON_EVENT = "onEvent";
  private final EventDispatcher dispatcher = new EventDispatcher();
  private final Map<String, NamespaceEventSinkRegistry<NamespaceKeyAccessEvent>> keyAccessRegistries =
      new HashMap<>();
  private final Map<String, NamespaceEventSinkRegistry<NamespaceKeyChangeEvent>> keyChangeRegistries =
      new HashMap<>();
  private BBjNamespace ns;

  private final NamespaceEventSinkRegistry<NamespaceAccessEvent> accessRegistry =
      new NamespaceEventSinkRegistry<>(new NamespaceAccessEventSink(this, dispatcher),
          NamespaceAccessEvent.class);

  private final NamespaceEventSinkRegistry<NamespaceChangeEvent> changeRegistry =
      new NamespaceEventSinkRegistry<>(new NamespaceChangeEventSink(this, dispatcher),
          NamespaceChangeEvent.class);

  /**
   * Sets the syntax value of a named object within this namespace.
   *
   * <p>
   * If the requested Object is currently locked by another namespace then the method will wait up
   * to timeout milliseconds for the lock to release. If the lock is still in effect after that
   * time, then an exception will be thrown.
   * </p>
   *
   * @param key Specifies the name of the named object.
   * @param value Specifies the value of the named object.
   * @param timeout Time in milliseconds to wait if requested Object is currently locked.
   *
   * @throws NamespaceLockedException if the variable is locked
   */
  public void put(String key, Object value, long timeout) throws NamespaceLockedException {
    try {
      ns.setValue(key, value, timeout);
    } catch (BBjException e) {
      throw new NamespaceLockedException(e);
    }
  }

  /**
   * Sets the syntax value of a named object within this namespace.
   *
   * <p>
   * If the requested Object is currently locked by another namespace then the method will wait up
   * to <code>20</code> milliseconds for the lock to release. If the lock is still in effect after
   * that time, then an exception will be thrown.
   * </p>
   *
   * @param key Specifies the name of the named object.
   * @param value Specifies the value of the named object.
   *
   * @throws NamespaceLockedException if the variable is locked
   *
   * @see #put(String, Object, long)
   */
  public void put(String key, Object value) throws NamespaceLockedException {
    put(key, value, 20);
  }

  /**
   * Atomically locks, sets a value, and then removes the lock.
   *
   * @param key Specifies the name of the named object.
   * @param value Specifies the value of the named object.
   * @param timeout Time in milliseconds to wait if the requested object is locked.
   *
   * @throws NamespaceLockedException if the variable remains locked beyond the timeout.
   */
  public void atomicPut(String key, Object value, long timeout) throws NamespaceLockedException {
    try {
      setLock(key, timeout);
      put(key, value, timeout);
    } finally {
      removeLock(key);
    }
  }

  /**
   * Atomically locks, sets a value, and then removes the lock.
   *
   * @param key Specifies the name of the named object.
   * @param value Specifies the value of the named object.
   *
   * @throws NamespaceLockedException if the variable remains locked beyond the timeout.
   */
  public void atomicPut(String key, Object value) throws NamespaceLockedException {
    atomicPut(key, value, 20);
  }

  /**
   * Returns the value for the named namespace object.
   *
   * @param key Specifies the named object for which the value is to be retrieved.
   * @return the variable content
   *
   * @throws NoSuchElementException if the variable does not exist in the namespace
   */
  public Object get(String key) {
    try {
      return ns.getValue(key);
    } catch (BBjException e) {
      throw new NoSuchElementException(e);
    }
  }

  /**
   * Returns the value for the named namespace object, or the default value if the object does not
   * exist.
   *
   * @param key Specifies the named object for which the value is to be retrieved.
   * @param defaultValue The default value to return if the object does not exist.
   * @return the variable content or the default value
   */
  public Object getOrDefault(String key, Object defaultValue) {
    try {
      return get(key);
    } catch (NoSuchElementException e) {
      return defaultValue;
    }
  }

  /**
   * Checks if the namespace contains the specified key.
   *
   * @param key The key to check for
   * @return true if the namespace contains the key, false otherwise
   */
  public boolean contains(String key) {
    try {
      @SuppressWarnings("unused")
      Object value = get(key);
      return true;
    } catch (NoSuchElementException e) {
      return false;
    }
  }

  /**
   * Computes the value for the specified key if it is absent.
   *
   * @param key The key to compute the value for
   * @param mappingFunction The function to compute the value
   * @return the current (existing or computed) value associated with the specified key
   *
   * @throws NamespaceLockedException if the key is locked
   */
  public Object computeIfAbsent(String key, Function<String, Object> mappingFunction)
      throws NamespaceLockedException {
    try {
      return get(key);
    } catch (NoSuchElementException e) {
      Object value = mappingFunction.apply(key);
      put(key, value);
      return value;
    }
  }

  /**
   * Removes an object from the namespace.
   *
   * <p>
   * If the requested Object is currently locked by another namespace then this method will wait up
   * to timeout milliseconds for the lock to release. If the lock is still in effect after that
   * time, then an exception will be thrown.
   * </p>
   *
   * @param key Specifies the name of the named object.
   * @param timeout Time in milliseconds to wait if requested Object is currently locked.
   *
   * @throws NamespaceLockedException if the variable is locked
   */
  public void remove(String key, long timeout) throws NamespaceLockedException {
    try {
      ns.removeValue(key, timeout);
    } catch (BBjException e) {
      throw new NamespaceLockedException(e);
    }
  }

  /**
   * Removes an object from the namespace.
   *
   * <p>
   * If the requested Object is currently locked by another namespace then this method will wait up
   * to <code>20</code> milliseconds for the lock to release. If the lock is still in effect after
   * that time, then an exception will be thrown.
   * </p>
   *
   * @param key Specifies the name of the named object.
   *
   * @throws NamespaceLockedException if the variable is locked
   * @see #remove(String, long)
   */
  public void remove(String key) throws NamespaceLockedException {
    remove(key, 20);
  }

  /**
   * Retrieves the keys of all elements currently contained in the namespace.
   *
   * @return a set containing all keys in the underlying namespace.
   */
  public Set<String> keySet() {
    BBjVector tmp;
    HashSet<String> keyset = new HashSet<>();

    try {
      tmp = ns.getKeys();
    } catch (BBjException e) {
      return keyset;
    }

    for (Object o : tmp) {
      keyset.add(o.toString());
    }

    return keyset;
  }

  /**
   * Gets the variable count in this namespace.
   *
   * @return the variable count in this namespace
   */
  public int size() {
    try {
      return ns.getKeys().size();
    } catch (BBjException e) {
      return 0;
    }
  }

  /**
   * Clears all variables in the namespace.
   */
  public void clear() {
    ns.clear();
  }

  /**
   * Locks a specified variable within the namespace.
   *
   * @param key Specifies the name of the object to be locked.
   * @param timeout Number of milliseconds to wait for lock.
   *
   * @throws NamespaceLockedException if the variable is already locked by a different namespace and
   *         remains locked longer than the timeout.
   */
  public void setLock(String key, long timeout) throws NamespaceLockedException {
    try {
      ns.setLock(key, timeout);
    } catch (BBjException e) {
      throw new NamespaceLockedException();
    }
  }

  /**
   * Removes any lock held by this.
   *
   * @param key The name of the variable for which the lock is to be removed.
   */
  public void removeLock(String key) {
    try {
      ns.removeLock(key);
    } catch (BBjException e) {
      // ignore
    }
  }

  /**
   * Gets the name of this namespace.
   *
   * @return the name of this namespace.
   */
  public String getName() {
    return ns.getName();
  }

  /**
   * Adds a {@link NamespaceAccessEvent} listener for the namespace.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<NamespaceAccessEvent> addAccessListener(
      EventListener<NamespaceAccessEvent> listener) {
    return accessRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addAccessListener(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<NamespaceAccessEvent> onAccess(
      EventListener<NamespaceAccessEvent> listener) {
    return addAccessListener(listener);
  }

  /**
   * Adds a {@link NamespaceChangeEvent} listener for the namespace.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<NamespaceChangeEvent> addChangeListener(
      EventListener<NamespaceChangeEvent> listener) {
    return changeRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addChangeListener(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<NamespaceChangeEvent> onChange(
      EventListener<NamespaceChangeEvent> listener) {
    return addChangeListener(listener);
  }

  /**
   * Adds a {@link NamespaceKeyAccessEvent} listener for the namespace.
   *
   * @param key the key to listen for
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<NamespaceKeyAccessEvent> addKeyAccessListener(String key,
      EventListener<NamespaceKeyAccessEvent> listener) {
    // create a sink for each key
    NamespaceEventSinkRegistry<NamespaceKeyAccessEvent> registry =
        keyAccessRegistries.computeIfAbsent(key, k -> {
          NamespaceKeyAccessEventSink sink = new NamespaceKeyAccessEventSink(this, key, dispatcher);
          return new NamespaceEventSinkRegistry<>(sink, NamespaceKeyAccessEvent.class);
        });

    return registry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addKeyAccessListener(String, EventListener)}.
   *
   * @param key the key to listen for
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<NamespaceKeyAccessEvent> onKeyAccess(String key,
      EventListener<NamespaceKeyAccessEvent> listener) {
    return addKeyAccessListener(key, listener);
  }

  /**
   * Adds a {@link NamespaceKeyChangeEvent} listener for the namespace.
   *
   * @param key the key to listen for
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<NamespaceKeyChangeEvent> addKeyChangeListener(String key,
      EventListener<NamespaceKeyChangeEvent> listener) {
    // create a sink for each key
    NamespaceEventSinkRegistry<NamespaceKeyChangeEvent> registry =
        keyChangeRegistries.computeIfAbsent(key, k -> {
          NamespaceKeyChangeEventSink sink = new NamespaceKeyChangeEventSink(this, key, dispatcher);
          return new NamespaceEventSinkRegistry<>(sink, NamespaceKeyChangeEvent.class);
        });

    return registry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addKeyChangeListener(String, EventListener)}.
   *
   * @param key the key to listen for
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<NamespaceKeyChangeEvent> onKeyChange(String key,
      EventListener<NamespaceKeyChangeEvent> listener) {
    return addKeyChangeListener(key, listener);
  }

  final void setBbjNamespace(BBjNamespace ns) {
    this.ns = ns;
  }

  final BBjNamespace getBbjNamespace() {
    return ns;
  }

  /**
   * Get the bridge object.
   *
   * @return the bridge object
   */
  final WebforjBBjBridge getBridge() {
    return Environment.getCurrent().getBridge();
  }
}
