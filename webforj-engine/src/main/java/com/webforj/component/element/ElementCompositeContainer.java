package com.webforj.component.element;

import com.webforj.component.Component;
import com.webforj.concern.HasComponents;
import java.util.List;

/**
 * Represents an abstract base class for Element container components. This class extends
 * {@link ElementComposite} and implements {@link HasComponents}, thus providing a structured way to
 * handle a collection of components. It is designed to be extended by specific container components
 * that need to manage their child components in a centralized manner.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public abstract class ElementCompositeContainer extends ElementComposite implements HasComponents {
  /**
   * {@inheritDoc}
   */
  @Override
  public void add(Component... components) {
    getElement().add(components);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void remove(Component... components) {
    getElement().remove(components);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeAll() {
    getElement().removeAll();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<Component> getComponents() {
    return getElement().getComponents();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Component getComponent(String id) {
    return getElement().getComponent(id);
  }
}
