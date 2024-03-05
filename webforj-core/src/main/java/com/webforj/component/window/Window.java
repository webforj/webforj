package com.webforj.component.window;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.Component;
import com.webforj.component.DwcContainer;
import com.webforj.component.event.EventSinkListenerRegistry;
import com.webforj.component.window.event.WindowClickEvent;
import com.webforj.component.window.sink.WindowClickEventSink;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;

/**
 * An abstract class representing a window in the application.
 *
 * <p>
 * Windows can contain other UI components. This class provides a foundation for creating and
 * managing windows.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public abstract class Window extends DwcContainer<Window> {

  private BBjWindow wnd;
  private final EventSinkListenerRegistry<WindowClickEvent> clickEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new WindowClickEventSink(this, getEventDispatcher()),
          WindowClickEvent.class);

  static {
    WindowAccessor.setDefault(new WindowAccessorImpl());
  }

  /**
   * Adds a {@link WindowClickEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<WindowClickEvent> addClickListener(
      EventListener<WindowClickEvent> listener) {
    return clickEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addClickListener(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public ListenerRegistration<WindowClickEvent> onClick(EventListener<WindowClickEvent> listener) {
    return addClickListener(listener);
  }

  /**
   * This method gets the underlying original BBj window. It's package private and can only be
   * accessed through the WindowAccessor. No API user / customer should ever work directly with BBj
   * windows.
   *
   * @return the underlying BBj Window
   */
  BBjWindow getBbjWindow() {
    return wnd;
  }

  /**
   * Sets the underling BBj Window.
   *
   * @param control the BBj Window to set.
   */
  protected void setBbjWindow(BBjWindow window) {
    this.wnd = window;
    setControl(window);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void attachControlCallbacks() {
    super.attachControlCallbacks();
    clickEventSinkListenerRegistry.attach();
  }

  /**
   * {@inheritDoc}x
   */
  @Override
  protected void onCreate(Window window) {
    // pass
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doAdd(Component component) {
    try {
      ComponentAccessor.getDefault().create(component, this);
    } catch (IllegalAccessException e) {
      throw new WebforjRuntimeException("Failed to add component", e);
    }
  }
}
