package org.dwcj.component;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.window.Window;

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

  private final Map<String, Object> userData = new HashMap<>();
  private String uuid = "";
  private boolean attached = false;
  private boolean destroyed = false;
  private Window window;

  static {
    ComponentAccessor.setDefault(new ComponentAccessorImpl());
  }

  /**
   * Retrieves a unique component ID generated on the server.
   *
   * <p>
   * Each component receives a distinct ID upon initialization, serving as its server-side
   * identifier. If the component is destroyed and then recreated, it obtains a fresh unique ID. If
   * the component is destroyed, this method returns null.
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
   * Allows users to include additional information within the component.
   *
   * <p>
   * Added information are available only on the server side of the component and are not sent to
   * the client.
   * </p>
   *
   * @param key Key of the data
   * @param data Desired piece of information
   *
   * @return The component itself
   */
  public Component setUserData(String key, Object data) {
    this.userData.put(key, data);
    return this;
  }

  /**
   * Retrieves user-included information from the component.
   *
   * @param key Key of the data
   * @return Desired piece of user data
   */
  public Object getUserData(String key) {
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
  }

  /**
   * Checks whether the component is destroyed.
   *
   * @return True if the component is destroyed, false otherwise
   */
  public final boolean isDestroyed() {
    return this.destroyed;
  }

  /**
   * Checks whether the component is attached to a window.
   *
   * @return True if the component is attached to a window, false otherwise
   */
  public final boolean isAttached() {
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
   * a window, those changes should be recorded and replayed within the {@code catchUp} method. This
   * ensures that the component behaves as expected with the specified properties when displayed on
   * the window.
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
  final void create(Window window) {
    if (this.isAttached()) {
      return;
    }

    this.window = window;
    this.onCreate(window);
    this.attached = true;
    this.onAttach();
  }
}
