package com.webforj.concern;

import com.webforj.component.Component;

/**
 * An interface that allows components to set and retrieve HTML content in a way that makes sense
 * for the specific component.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasHtml<T extends Component> {

  /**
   * Retrieves the HTML content of the component.
   *
   * @return the HTML content of the component.
   */
  public String getHtml();

  /**
   * Sets the HTML content of the component. Each component implementing this interface has an HTML
   * content property, which may be rendered in different ways.
   *
   * @param html the HTML content to set for the component.
   * @return the component itself.
   */
  public T setHtml(String html);
}
