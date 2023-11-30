package org.dwcj.component;

import java.util.List;
import java.util.function.Consumer;
import org.dwcj.concern.HasComponents;

/**
 * Represents an abstract base class for Dwc container components. This class extends
 * {@link DwcComponent} and implements {@link HasComponents}, thus providing a structured way to
 * handle a collection of components. It is designed to be extended by specific container components
 * that need to manage their child components in a centralized manner.
 *
 * @param <T> the type parameter representing the specific subtype of {@code DwcContainer} for
 *        fluent method chaining
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public abstract class DwcContainer<T extends DwcContainer<T>> extends DwcComponent<T>
    implements HasComponents {
  private final Consumer<Component> addConsumer;
  private final ComponentRegistry registry;

  /**
   * Constructs a new {@code DwcContainer}. Initializes the consumer for adding components and the
   * component registry to manage components.
   */
  protected DwcContainer() {
    super();
    addConsumer = this::doAdd;
    registry = new ComponentRegistry(this, addConsumer, allowMultipleAttach());
  }

  /**
   * Returns {@code true} if the container allows attaching a component to multiple containers.
   * Otherwise, returns {@code false}.
   *
   * @return {@code true} if the container allows attaching a component to multiple containers.
   *         Otherwise, returns {@code false}.
   */
  protected boolean allowMultipleAttach() {
    return false;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void add(Component... components) {
    registry.add(components);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void remove(Component... components) {
    registry.remove(components);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeAll() {
    registry.removeAll();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<Component> getComponents() {
    return registry.getComponents();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Component getComponent(String id) {
    return registry.getComponent(id);
  }

  /**
   * Provides access to the {@link ComponentRegistry} associated with this container.
   *
   * @return the {@code ComponentRegistry} associated with this container
   */
  protected final ComponentRegistry getComponentRegistry() {
    return registry;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    super.onAttach();

    if (registry.getComponentCount() > 0) {
      registry.getComponents().forEach(addConsumer);
    }
  }

  /**
   * Abstract method for adding a single component to the container. This method must be implemented
   * by subclasses to define the specific behavior of component addition.
   *
   * @param component the component to add to the container
   */
  protected abstract void doAdd(Component component);
}
