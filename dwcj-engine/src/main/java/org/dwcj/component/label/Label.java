package org.dwcj.component.label;

import com.basis.bbj.proxies.sysgui.BBjStaticText;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.TextAlignable;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.MouseExitEvent;
import org.dwcj.component.event.RightMouseDownEvent;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.dwcj.component.event.sink.RightMouseDownEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.BBjFunctionalityHelper;


/** A label object. */
public final class Label extends AbstractDwcComponent implements TextAlignable {

  private EventDispatcher dispatcher = new EventDispatcher();
  private MouseEnterEventSink mouseEnterEventSink;
  private MouseExitEventSink mouseExitEventSink;
  private RightMouseDownEventSink rightMouseDownEventSink;
  private boolean lineWrap = true;

  /**
   * Default Constructor to automatically create an empty label.
   */
  public Label() {
    this("");
  }

  /**
   * Constructor used to give the label initial text.
   *
   * @param text String value for initial display text
   */
  public Label(String text) {
    setText(text);
  }

  /**
   * Constructor used to give the label initial text and wether it is linewrapped or not.
   *
   * @param text String value for initial display text
   * @param wrap Boolean value for linewrapping.
   */
  public Label(String text, boolean wrap) {
    this(text);
    this.setWrap(wrap);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void create(AbstractWindow p) throws DwcjRuntimeException {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags = BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), true);
      ctrl = w.addStaticText(getText(), flags);
      this.mouseEnterEventSink = new MouseEnterEventSink(this, dispatcher);
      this.mouseExitEventSink = new MouseExitEventSink(this, dispatcher);
      this.rightMouseDownEventSink = new RightMouseDownEventSink(this, dispatcher);
      catchUp();
    } catch (Exception e) {
      throw new DwcjRuntimeException(e);
    }
  }

  /**
   * Adds a MouseEnter event for the Label component.
   *
   * @param listener the event listener to be added
   * @return The Label itself
   */
  public Label addMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseEnterEvent.class) == 0) {
      this.mouseEnterEventSink.setCallback();
    }
    dispatcher.addEventListener(MouseEnterEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addMouseEnterListener method.
   *
   * @see Label#addMouseEnterListener(EventListener)
   * @param listener the event listener to be added
   * @return The Label itself
   */
  public Label onMouseEnter(EventListener<MouseEnterEvent> listener) {
    return addMouseEnterListener(listener);
  }

  /**
   * Removes a MouseEnter event from the Label component.
   *
   * @param listener the event listener to be removed
   * @return The Label itself
   */
  public Label removeMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    dispatcher.removeEventListener(MouseEnterEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseEnterEvent.class) == 0) {
      this.mouseEnterEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a MouseExit event for the Label component.
   *
   * @param listener the event listener to be added
   * @return The Label itself
   */
  public Label addMouseExitListener(EventListener<MouseExitEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseExitEvent.class) == 0) {
      this.mouseExitEventSink.setCallback();
    }
    dispatcher.addEventListener(MouseExitEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addMouseExitListener method.
   *
   * @see Label#addMouseExitListener(EventListener)
   * @param listener the event listener to be added
   * @return The Label itself
   */
  public Label onMouseExit(EventListener<MouseExitEvent> listener) {
    return addMouseExitListener(listener);
  }

  /**
   * Removes a MouseExit event from the Label component.
   *
   * @param listener the event listener to be removed
   * @return The Label itself
   */
  public Label removeMouseExitListener(EventListener<MouseExitEvent> listener) {
    dispatcher.removeEventListener(MouseExitEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseExitEvent.class) == 0) {
      this.mouseExitEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a MouseExit event for the Label component.
   *
   * @param listener the event listener to be added
   * @return The Label itself
   */
  public Label addRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(RightMouseDownEvent.class) == 0) {
      this.rightMouseDownEventSink.setCallback();
    }
    dispatcher.addEventListener(RightMouseDownEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addRightMouseDownListener method.
   *
   * @see Label#addRightMouseDownListener(EventListener)
   * @param listener the event listener to be added
   * @return The Label itself
   */
  public Label onRightMouseDown(EventListener<RightMouseDownEvent> listener) {
    return addRightMouseDownListener(listener);
  }

  /**
   * Removes a RightMouseDown event from the Label component.
   *
   * @param listener the event listener to be removed
   * @return The Label itself
   */
  public Label removeRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    dispatcher.removeEventListener(RightMouseDownEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(RightMouseDownEvent.class) == 0) {
      this.rightMouseDownEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Sets whether the lines will be wrapped in the label.
   *
   * @param wrap - Specifies whether the lines will be wrapped (false = Not Wrapped, true = Wrapped)
   * @return Returns this
   */
  public Label setWrap(Boolean wrap) throws DwcjRuntimeException {
    if (this.ctrl != null) {
      try {
        ((BBjStaticText) this.ctrl).setLineWrap(wrap);
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }
    this.lineWrap = wrap;
    return this;
  }

  /**
   * Returns whether lines are wrapped in the label.
   *
   * @return Returns whether the lines are wrapped in the component (false = Not Wrapped, true =
   *         Wrapped).
   */
  public boolean isWrap() {
    return this.lineWrap;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Label setTextAlignment(Alignment alignment) throws DwcjRuntimeException {
    if (this.ctrl != null) {
      try {
        ((BBjStaticText) this.ctrl).setAlignment(alignment.getValue());
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }
    this.textAlignment = alignment;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Alignment getTextAlignment() {
    return this.textAlignment;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Label setText(String text) {
    super.setText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Label setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Label setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Label setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Label setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Label addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Label removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }

  private void eventCatchUp() {
    if (this.dispatcher.getListenersCount(MouseEnterEvent.class) > 0) {
      this.mouseEnterEventSink.setCallback();
    }

    if (this.dispatcher.getListenersCount(MouseExitEvent.class) > 0) {
      this.mouseExitEventSink.setCallback();
    }

    if (this.dispatcher.getListenersCount(RightMouseDownEvent.class) > 0) {
      this.rightMouseDownEventSink.setCallback();
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
    if (!this.lineWrap) {
      this.setWrap(lineWrap);
    }

    if (this.textAlignment != Alignment.MIDDLE) {
      this.setTextAlignment(this.textAlignment);
    }
  }
}
