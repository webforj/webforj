package org.dwcj.component.window;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.Component;
import org.dwcj.component.DwcContainer;
import org.dwcj.component.event.EventSinkListenerRegistry;
import org.dwcj.component.window.event.WindowClickEvent;
import org.dwcj.component.window.sink.WindowClickEventSink;
import org.dwcj.dispatcher.EventListener;
import org.dwcj.dispatcher.ListenerRegistration;
import org.dwcj.exceptions.DwcjRuntimeException;

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
  BBjWindow getBBjWindow() {
    return wnd;
  }

  /**
   * Sets the underling BBj Window.
   *
   * @param control the BBj Window to set.
   */
  protected void setBBjWindow(BBjWindow window) {
    this.wnd = window;
    setControl(window);
  }

  /**
   * {@inheritDoc}
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
      throw new DwcjRuntimeException("Failed to add component", e);
    }
  }
}
