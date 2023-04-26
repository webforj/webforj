package org.dwcj.component.tree;

import com.basis.bbj.proxies.sysgui.BBjTree;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.BlurEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.FocusEvent;
import org.dwcj.component.event.sink.BlurEventSink;
import org.dwcj.component.event.sink.FocusEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.util.BBjFunctionalityHelper;

/**
 * A Tree control.
 */
public final class Tree extends AbstractDwcComponent {

  private BBjTree bbjTree;
  private final EventDispatcher dispatcher = new EventDispatcher();
  private FocusEventSink focusEventSink;
  private BlurEventSink blurEventSink;

  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags = 
        BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      ctrl = w.addTree(flags);
      createEventSinks();
      bbjTree = (BBjTree) ctrl;
      catchUp();
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to create Tree", e);
    }
  }

  private void createEventSinks() {
    this.focusEventSink = new FocusEventSink(this, dispatcher);
    this.blurEventSink = new BlurEventSink(this, dispatcher);
  }

  /**
   * Adds a focus event for the Tree component.
   *
   * @param listener The event
   * @return The component itself
   */
  public Tree addFocusListener(EventListener<FocusEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(FocusEvent.class) == 0) {
      this.focusEventSink.setCallback();
    }
    dispatcher.addEventListener(FocusEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addFocusListener method.
   *
   * @see Tree #addFocusListener(EventListener)
   * @param listener A method to receive the focus event
   * @return the component itself
   */
  public Tree onFocus(EventListener<FocusEvent> listener) {
    return addFocusListener(listener);
  }

  /**
   * Removes a focus event from the Tree component.
   *
   * @param listener The event to be removed
   * @return The component itself
   */
  public Tree removeFocusListener(EventListener<FocusEvent> listener) {
    dispatcher.removeEventListener(FocusEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(FocusEvent.class) == 0) {
      this.focusEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a blur event for the Tree component. A blur event fires when a component looses focus.
   *
   * @param listener The event
   * @return The component itself
   */
  public Tree addBlurListener(EventListener<BlurEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(BlurEvent.class) == 0) {
      this.blurEventSink.setCallback();
    }
    dispatcher.addEventListener(BlurEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addBlurListener method.
   *
   * @see Tree #addBlurListener(EventListener)
   * @param listener A method to receive the blur event
   * @return the component itself
   */
  public Tree onBlur(EventListener<BlurEvent> listener) {
    return addBlurListener(listener);
  }

  /**
   * Removes a blur event from the Tree component. Fires when a component looses focus.
   *
   * @param listener The event to be removed
   * @return The component itself
   */
  public Tree removeBlurListener(EventListener<BlurEvent> listener) {
    dispatcher.removeEventListener(BlurEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(BlurEvent.class) == 0) {
      this.blurEventSink.removeCallback();
    }
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree setText(String text) {
    super.setText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree setEnabled(Boolean enabled) {
    super.setEnabled(enabled);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree removeAttribute(String attribute) {
    super.removeAttribute(attribute);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree setId(String elementId) {
    super.setId(elementId);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree setUserData(String key, Object userData) {
    super.setUserData(key, userData);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree removeStyle(String property) {
    super.removeStyle(property);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree setProperty(String property, Object value) {
    super.setProperty(property, value);
    return this;
  }

  private void eventCatchUp() {
    if (this.dispatcher.getListenersCount(FocusEvent.class) > 0) {
      this.focusEventSink.setCallback();
    }

    if (this.dispatcher.getListenersCount(BlurEvent.class) > 0) {
      this.blurEventSink.setCallback();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp())) {
      throw new IllegalAccessException("catchUp cannot be called twice");
    }
    super.catchUp();
    eventCatchUp();
  }
}
