package com.webforj.router;

import com.webforj.component.Component;
import com.webforj.component.ComponentLifecycleObserver;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.concern.HasComponents;
import com.webforj.concern.HasVisibility;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

/**
 * The {@code PersistentRouteOutlet} class is a {@link RouteOutlet} implementation that manages
 * route components by displaying the current component while hiding others within a container. It
 * persists components between navigation, avoiding unnecessary creation and destruction.
 *
 * <p>
 * This class allows for custom handling of how components are added to the container by accepting a
 * consumer in the constructor. This provides flexibility in managing the component hierarchy
 * without altering the core functionality of the outlet.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 *
 * @see RouteOutlet
 */
public class PersistentRouteOutlet implements RouteOutlet {
  private final Map<Class<? extends Component>, Component> componentCache = new HashMap<>();
  private final Component container;
  private final Consumer<Component> addComponentFunction;

  /**
   * Constructs a new {@code PersistentRouteOutlet} with the specified container component and a
   * function to handle adding components.
   *
   * @param container The container component to display the route content within.
   * @param addComponentConsumer A consumer that handles adding a component to the container.
   */
  public PersistentRouteOutlet(Component container, Consumer<Component> addComponentConsumer) {
    this.container = container;
    this.addComponentFunction = addComponentConsumer;
    container.whenAttached().thenAccept(
        e -> Router.getCurrent().onNavigate(ev -> update(ev.getContext().getComponent())));
  }

  /**
   * Constructs a new {@code PersistentRouteOutlet} with the specified container component.
   *
   * @param container The container component to display the route content within.
   */
  public PersistentRouteOutlet(Component container) {
    this(container, new DefaultAddComponentConsumer(container));
  }

  /**
   * Displays the content of the specified route component. If the component is not already
   * attached, it uses the provided function to add it to the container.
   *
   * @param component The component to display.
   */
  @Override
  public void showRouteContent(Component component) {
    componentCache.put(component.getClass(), component);
    if (!component.isAttached()) {
      addComponentFunction.accept(component);

      // Remove the component from the cache when it is destroyed
      component.addLifecycleObserver((c, e) -> {
        if (e == ComponentLifecycleObserver.LifecycleEvent.DESTROY) {
          componentCache.remove(c.getClass());
        }
      });
    }
  }

  /**
   * No operation is performed in this implementation, as components are persisted and visibility is
   * managed on navigation.
   *
   * @param component The component to hide.
   */
  @Override
  public void removeRouteContent(Component component) {
    // no-op
  }

  /**
   * Retrieves an unmodifiable map of the cached route components.
   *
   * @return An unmodifiable map of the cached route components.
   */
  public Map<Class<? extends Component>, Component> getComponents() {
    return Collections.unmodifiableMap(componentCache);
  }

  /**
   * Retrieves the container component.
   *
   * @return The container component.
   */
  public Component getContainer() {
    return container;
  }

  /**
   * Updates the visibility of components based on the current navigation. It hides all other
   * components and shows the current one.
   *
   * @param component The component to display.
   */
  private void update(Component component) {
    componentCache.values().forEach(c -> {
      if (!c.equals(component) && c instanceof HasVisibility) {
        ((HasVisibility<?>) c).setVisible(false);
      }
    });

    if (component instanceof HasVisibility) {
      ((HasVisibility<?>) component).setVisible(true);
    }
  }

  /**
   * The default function for adding a component to a container.
   */
  static final class DefaultAddComponentConsumer implements Consumer<Component> {
    private final Component container;

    DefaultAddComponentConsumer(Component container) {
      this.container = container;
    }

    @Override
    public void accept(Component component) {
      getComponentsManager(container).add(component);
    }

    /**
     * Retrieves the components manager for the specified container.
     *
     * @param container The container component.
     * @return The components manager.
     */
    public HasComponents getComponentsManager(Component container) {
      if (container instanceof HasComponents hasComponents) {
        return hasComponents;
      } else if (container instanceof Composite<?> composite) {
        Component boundComponent = ComponentUtil.getBoundComponent(composite);
        if (boundComponent instanceof HasComponents hasBoundComponents) {
          return hasBoundComponents;
        } else {
          return getComponentsManager(boundComponent);
        }
      } else {
        throw new IllegalStateException("Container does not support adding components.");
      }
    }
  }
}
