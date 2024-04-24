package com.webforj.data.concern;

/**
 * An interface for components that can receive focus.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @since 23.05
 */
public interface FocusAcceptorAware<T> {

  /**
   * Gives focus to the component when it is added to a window.
   *
   * @return The component itself.
   */
  public T focus();
}
