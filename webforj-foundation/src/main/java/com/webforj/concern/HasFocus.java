package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.event.BlurEvent;
import com.webforj.component.event.FocusEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;

/**
 * An interface for managing focus behavior of components.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasFocus<T extends Component> {

  /**
   * Gives focus to the component when it is added to a window.
   *
   * @return The component itself.
   */
  public T focus();

  /**
   * Checks whether the user can focus to the component using the tab key.
   *
   * <p>
   * It's important to note that if the component is set as focusable but is disabled, the user
   * won't be able to navigate to the component using the Tab key or focus it programmatically.
   * However, the component will still be included in the tab order.
   * </p>
   *
   * @return true if the user can navigate to the component with the Tab key, false if not.
   */
  public boolean isFocusable();

  /**
   * Sets whether the user can focus to the component using the tab key.
   *
   * <p>
   * This method allows you to enable or disable focusing the component. When enabled, the component
   * will be part of the tab order. When disabled the component is excluded from tab navigation.
   * </p>
   *
   * @param focusable true to enable focusing the component, false to disable it.
   * @return the component itself.
   */
  public T setFocusable(boolean focusable);

  /**
   * Adds a {@link FocusEvent} listener to the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<FocusEvent> addFocusListener(EventListener<FocusEvent> listener);

  /**
   * Alias for {@link #addFocusListener(EventListener) addFocusListener}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public default ListenerRegistration<FocusEvent> onFocus(EventListener<FocusEvent> listener) {
    return addFocusListener(listener);
  }

  /**
   * Adds a {@link BlurEvent} listener to the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<BlurEvent> addBlurListener(EventListener<BlurEvent> listener);

  /**
   * Alias for {@link #addBlurListener(EventListener) addBlurListener}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public default ListenerRegistration<BlurEvent> onBlur(EventListener<BlurEvent> listener) {
    return addBlurListener(listener);
  }
}
