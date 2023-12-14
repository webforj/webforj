package org.dwcj.concern;

import java.util.List;
import org.dwcj.component.Component;

/**
 * An interface for components that act like containers for other components. Basically, it's a way
 * to organize components that are the children of another component within the component itself.
 * This interface offers a clear set of functions to add or remove these child components as needed.
 *
 * <p>
 * This is something you'll usually find in windows and layout components or other components that
 * are meant to hold more components inside them. But this isn't something you would find in
 * non-layout components, like input fields and such.
 * </p>
 *
 * @param <T> the type of the components that this component can hold.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasComponents {

  /**
   * Adds the given components as children of this component.
   *
   * @param components the components to add.
   *
   * @throws NullPointerException if the given components is null.
   * @throws IllegalArgumentException if any of the given components cannot be added by this
   *         component.
   * @throws IllegalStateException if any of the given components is destroyed.
   */
  public void add(Component... components);

  /**
   * Removes the given components as children of this component.
   *
   * @param components the components to remove.
   *
   * @throws NullPointerException if the given components is null.
   */
  public void remove(Component... components);

  /**
   * Removes all the children of this component.
   */
  public void removeAll();

  /**
   * Checks whether the given component is a child of this component.
   *
   * @return the component itself.
   */
  public default boolean hasComponent(Component component) {
    return component != null && getComponents().contains(component);
  }

  /**
   * Returns the children of this component.
   *
   * @return the children of this component.
   */
  public List<Component> getComponents();

  /**
   * Returns the number of children of this component.
   *
   * @return the number of children of this component.
   */
  public default int getComponentCount() {
    return getComponents().size();
  }

  /**
   * Returns the child component with the given id.
   *
   * @param id the id of the child component to return.
   * @return the child component with the given id.
   */
  public Component getComponent(String id);
}

