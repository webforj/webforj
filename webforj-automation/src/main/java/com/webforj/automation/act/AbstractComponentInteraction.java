package com.webforj.automation.act;

import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.automation.find.ComponentTree;
import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.component.event.ComponentEvent;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * The base of the shipped interactions, holding the contract and what every interaction needs to
 * report an event the way the browser does.
 *
 * @param <C> the contract of the components the interaction serves
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public abstract class AbstractComponentInteraction<C> implements ComponentInteraction<C> {
  private final Class<C> contract;

  /**
   * Creates an interaction for the given contract.
   *
   * @param contract the class or interface of the components the interaction serves
   */
  @SuppressWarnings("unchecked")
  protected AbstractComponentInteraction(Class<?> contract) {
    this.contract = (Class<C>) contract;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final Class<C> getContract() {
    return contract;
  }

  /**
   * Returns the components that hold the given component, the nearest first.
   *
   * @param component the component
   *
   * @return the holders, empty when the component is not in the tree
   */
  static List<Component> findHolders(Component component) {
    for (Component root : ComponentTree.getRoots()) {
      Deque<Component> path = new ArrayDeque<>();
      if (findPath(root, component, path)) {
        path.removeFirst();
        return List.copyOf(path);
      }
    }

    return List.of();
  }

  /**
   * Delivers the given event to the listeners of its component, as if the browser sent it.
   *
   * @param event the event
   *
   * @throws IllegalStateException if the component of the event delivers no events to listeners
   */
  protected static void fire(ComponentEvent<?> event) {
    ComponentUtil.fireComponentEvent(event.getComponent(), event);
  }

  /**
   * Creates the rejection of an interaction with the given component.
   *
   * @param component the component the user cannot interact with
   * @param reason why, completing the sentence "The component X ..."
   *
   * @return the rejection to throw
   */
  protected static InteractionRejectedException reject(Component component, String reason) {
    return new InteractionRejectedException(component, reason);
  }

  /**
   * Returns the pointer details of the given options the way a control reports them.
   *
   * @param component the component the pointer is on
   * @param options the pointer details
   *
   * @return the event data
   */
  protected static Map<String, Object> mouseData(Component component, MouseOptions options) {
    Map<String, Object> data = new HashMap<>();
    data.put("mouseButton", toRuntimeButton(options.getButton()));
    data.put("screenX", options.getScreenX());
    data.put("screenY", options.getScreenY());
    data.put("x", options.getX());
    data.put("y", options.getY());
    data.put("altDown", options.isAltKey());
    data.put("cmdDown", options.isMetaKey());
    data.put("controlDown", options.isControlKey());
    data.put("shiftDown", options.isShiftKey());
    data.put("originalComponent", resolveOriginalComponent(component, options));

    return data;
  }

  /**
   * Returns the nearest component of the given type that holds the given component.
   *
   * @param <T> the type of the holder
   * @param component the component
   * @param type the class of the holder
   *
   * @return the holder, empty when no component of the type holds the component
   */
  protected static <T> Optional<T> findHolder(Component component, Class<T> type) {
    return findHolders(component).stream().filter(type::isInstance).map(type::cast).findFirst();
  }

  private static int toRuntimeButton(int button) {
    return button >= 0 && button <= 2 ? button + 1 : button;
  }

  private static Component resolveOriginalComponent(Component component, MouseOptions options) {
    Component original =
        options.getOriginalComponent() == null ? component : options.getOriginalComponent();
    while (original instanceof Composite<?>) {
      original = ComponentUtil.getBoundComponent(original);
    }

    return original;
  }

  private static boolean findPath(Component current, Component target, Deque<Component> path) {
    if (current == target) {
      path.addLast(current);
      return true;
    }

    for (Component child : ComponentTree.getChildren(current)) {
      if (findPath(child, target, path)) {
        path.addLast(current);
        return true;
      }
    }

    return false;
  }
}
