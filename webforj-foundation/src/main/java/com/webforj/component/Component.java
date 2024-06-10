package com.webforj.component;

import com.webforj.PendingResult;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.ComponentLifecycleObserver.LifecycleEvent;
import com.webforj.component.window.Window;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * The {@code Component} class serves as the foundation for all components within the engine.
 *
 * <p>
 * Subclasses of this class are expected to provide implementations for the {@code onCreate} and
 * {@code onDestroy} methods to define component-specific behavior.
 * </p>
 *
 * <p>
 * This class allows components to generate unique IDs, manage user-specific data, and handle their
 * lifecycle states.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public abstract class Component {
  private final Map<Object, Object> userData = new HashMap<>();
  private final List<ComponentLifecycleObserver> lifecycleObservers = new ArrayList<>();
  private String uuid = "";
  private String name = "";
  private boolean attached = false;
  private boolean destroyed = false;
  private Window window;
  private final List<PendingResult<Component>> whenAttachedResults = new ArrayList<>();

  static {
    ComponentAccessor.setDefault(new ComponentAccessorImpl());
  }

  /**
   * Retrieves a unique component ID generated on the server.
   *
   * <p>
   * Each component receives a distinct ID upon initialization, serving as its server-side
   * identifier.
   * </p>
   *
   * @return The component ID as a String.
   */
  public final String getComponentId() {
    if (isDestroyed()) {
      return null;
    }

    if (uuid.equals("")) {
      uuid = UUID.randomUUID().toString();
    }

    return uuid;
  }

  /**
   * Retrieves the ID of the component's client-side counterpart.
   *
   * <p>
   * If the component has a client-side counterpart, this method returns the ID of that component on
   * the client side. If the component does not have a client-side counterpart, this method returns
   * {@code null}.
   * </p>
   *
   * @return The ID of the component's client-side counterpart.
   * @since 23.06
   */
  public String getClientComponentId() {
    return null;
  }

  /**
   * Sets the name of the component.
   *
   * <p>
   * Sets the name of the component to the specified string. The component may use this name to
   * for accessibility purposes.
   * </p>
   *
   * @param name the string that is to be this component's name
   *
   * @return The component itself
   */
  public Component setName(String name) {
    this.name = name;
    return this;
  }

  /**
   * Retrieves the name of the component.
   *
   * <p>
   * Returns the name of this component. The default name is an empty string.
   * </p>
   *
   * @return The name of the component
   */
  public String getName() {
    return this.name;
  }

  /**
   * Allows users to include additional information within the component.
   *
   * <p>
   * Added information is available only on the server side of the component and is not sent to the
   * client.
   * </p>
   *
   * @param key Key of the data
   * @param data Desired piece of information
   *
   * @return The component itself
   */
  public Component setUserData(Object key, Object data) {
    this.userData.put(key, data);
    return this;
  }

  /**
   * Retrieves user-included information from the component.
   *
   * @param key Key of the data
   * @return Desired piece of user data
   */
  public Object getUserData(Object key) {
    return this.userData.get(key);
  }

  /**
   * Destroys the component and removes it from the panel.
   *
   * <p>
   * If the component is already destroyed, this method does nothing and returns immediately.
   * </p>
   */
  public final void destroy() {
    if (isDestroyed()) {
      return;
    }

    this.userData.clear();
    this.uuid = "";
    this.attached = false;
    this.destroyed = true;
    this.window = null;
    this.onDestroy();

    for (ComponentLifecycleObserver observer : lifecycleObservers) {
      observer.onComponentLifecycleEvent(this, LifecycleEvent.DESTROY);
    }
  }

  /**
   * Checks whether the component is destroyed.
   *
   * @return True if the component is destroyed, false otherwise
   */
  public boolean isDestroyed() {
    return this.destroyed;
  }

  /**
   * Checks whether the component is attached to a window.
   *
   * @return True if the component is attached to a window, false otherwise
   */
  public boolean isAttached() {
    return this.attached;
  }

  /**
   * Retrieves the window instance to which this component is attached.
   *
   * <p>
   * This method's result is only available after the component has been created and added to a
   * window. If the component has not been added to a window, this method returns null.
   * </p>
   *
   * @return The window instance to which this component is attached.
   */
  public final Window getWindow() {
    return this.window;
  }

  /**
   * Adds a listener to receive notifications about lifecycle events of the component.
   *
   * @param observer The listener to add
   */
  public void addLifecycleObserver(ComponentLifecycleObserver observer) {
    lifecycleObservers.add(observer);
  }

  /**
   * Removes a listener from receiving notifications about lifecycle events of the component.
   *
   * @param observer The listener to remove
   */
  public void removeLifecycleObserver(ComponentLifecycleObserver observer) {
    lifecycleObservers.remove(observer);
  }

  /**
   * Returns a {@link PendingResult} that completes when the named component is attached in the DOM.
   *
   * @return A {@link PendingResult} that completes when a component becomes attached. If a
   *         component has already been attached, the PendingResult will immediately complete.
   *
   * @since 23.06
   */
  public PendingResult<Component> whenAttached() {
    if (isAttached()) {
      return PendingResult.completedWith(this);
    }

    PendingResult<Component> result = new PendingResult<>();
    whenAttachedResults.add(result);
    return result;
  }

  /**
   * Creates the component and adds it to the given window.
   *
   * <p>
   * Note: It is the developer's responsibility to ensure that the component is added to the window.
   * </p>
   *
   * <p>
   * This method is called automatically when the window attempts to render the component and should
   * not be called directly by the API user.
   * </p>
   *
   * @param window The window to which this component is added.
   */
  protected abstract void onCreate(Window window);

  /**
   * The {@code onDestroy} method is called when a component is destroyed. It is used to clean up
   * any resources that the component might have allocated during its lifetime.
   *
   * <p>
   * For example, if the component has a timer used to update its display, the timer is stopped and
   * destroyed within the {@code onDestroy} method.
   * </p>
   *
   * <p>
   * Note: This method is called automatically when the component is destroyed and should not be
   * called directly by the API user.
   * </p>
   */
  protected abstract void onDestroy();

  /**
   * The {@code onAttach} method can be used to replay settings that the API user might have added
   * to a component before its creation and addition to a window. It allows the component to
   * incorporate any changes made to it between instantiation and its actual usage on a window.
   *
   * <p>
   * When a component is created, it is not fully functional until it is added to a window. The
   * {@code onAttach} method is called just after the component is created to ensure that any
   * changes or configurations made by the API user before adding the component are applied
   * correctly.
   * </p>
   *
   * <p>
   * For example, if the API user sets properties or attributes of the component before adding it to
   * a window, those changes should be recorded and replayed within the {@code onAttach} method.
   * This ensures that the component behaves as expected with the specified properties when
   * displayed on the window.
   * </p>
   */
  protected void onAttach() {}

  /**
   * Creates the component and adds it to the given window.
   *
   * <p>
   * This method is called by the {@link ComponentAccessorImpl} when the component is created and
   * added to a window. It should not be called directly.
   * </p>
   *
   * @param window The window to which this component is added.
   */
  protected final void create(Window window) {
    if (isDestroyed()) {
      throw new IllegalStateException("Cannot create a destroyed component");
    }

    if (isAttached()) {
      return;
    }

    this.window = window;

    onCreate(window);
    for (ComponentLifecycleObserver observer : lifecycleObservers) {
      observer.onComponentLifecycleEvent(this, LifecycleEvent.CREATE);
    }

    this.attached = true;
    this.onAttach();
    whenAttachedResults.forEach(r -> r.complete(this));
  }
}
