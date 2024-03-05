package com.webforj.component.html;

import com.webforj.component.Component;
import com.webforj.concern.HasComponents;
import java.util.List;

/**
 * Base class for a {@link Component} that represents a single built-in HTML element that can
 * contain child components or text.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public abstract class HtmlComponentContainer<T extends HtmlComponentContainer<T>>
    extends HtmlComponent<T> implements HasComponents {

  /**
   * Creates a new HtmlComponentContainer with given components.
   *
   * @param components the components to be added to the container
   */
  protected HtmlComponentContainer(Component... components) {
    add(components);
  }

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
