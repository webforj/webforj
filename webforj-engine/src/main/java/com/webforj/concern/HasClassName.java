package com.webforj.concern;

import com.webforj.component.Component;

/**
 * An interface that enables adding or removing CSS classes to a component.
 *
 * <p>
 * This interface provides methods for adding or removing CSS classes to/from a component's list of
 * CSS classes.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasClassName<T extends Component> {

  /**
   * Adds a CSS class to the list of CSS classes for the component.
   *
   * @param classNames the name of the CSS class to be added
   * @return the component itself after adding the class
   */
  public T addClassName(String... classNames);

  /**
   * Removes a CSS class from the list of CSS classes for the component.
   *
   * @param classNames the name of the CSS class to be removed
   * @return the component itself after removing the class
   */
  public T removeClassName(String... classNames);
}
