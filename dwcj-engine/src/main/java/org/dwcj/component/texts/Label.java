package org.dwcj.component.texts;

import com.basis.bbj.proxies.sysgui.BBjStaticText;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.MouseExitEvent;
import org.dwcj.component.event.RightMouseDownEvent;
import org.dwcj.component.event.sink.EventSinkListenerRegistry;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.dwcj.component.event.sink.RightMouseDownEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.concern.HasHorizontalAlignment;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.BBjFunctionalityHelper;

/** A label object. */
public final class Label extends AbstractDwcComponent implements HasHorizontalAlignment<Label> {

  private EventDispatcher dispatcher = new EventDispatcher();

  private EventSinkListenerRegistry<MouseEnterEvent> mouseEnterEventHandler =
      new EventSinkListenerRegistry<>(new MouseEnterEventSink(this, dispatcher),
          MouseEnterEvent.class);
  private EventSinkListenerRegistry<MouseExitEvent> mouseExitEventHandler =
      new EventSinkListenerRegistry<>(new MouseExitEventSink(this, dispatcher),
          MouseExitEvent.class);
  private EventSinkListenerRegistry<RightMouseDownEvent> rightMouseDownEventHandler =
      new EventSinkListenerRegistry<>(new RightMouseDownEventSink(this, dispatcher),
          RightMouseDownEvent.class);

  private boolean lineWrap = true;

  /**
   * Default Constructor to automatically create an empty label.
   */
  public Label() {
    this("", true);
  }

  /**
   * Constructor used to give the label initial text.
   *
   * @param text String value for initial display text
   */
  public Label(String text) {
    this(text, true);
  }

  /**
   * Constructor used to give the label initial text and wether it is linewrapped or not.
   *
   * @param text String value for initial display text
   * @param wrap Boolean value for linewrapping.
   */
  public Label(String text, boolean wrap) {
    setText(text);
    setWrap(wrap);
    setComponentDefaultHorizontalAlignment(Alignment.LEFT);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags = BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), true);
      setControl(w.addStaticText(getText(), flags));
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
    this.mouseEnterEventHandler.addEventListener(listener);
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
    this.mouseEnterEventHandler.removeEventListener(listener);
    return this;
  }

  /**
   * Adds a MouseExit event for the Label component.
   *
   * @param listener the event listener to be added
   * @return The Label itself
   */
  public Label addMouseExitListener(EventListener<MouseExitEvent> listener) {
    this.mouseExitEventHandler.addEventListener(listener);
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
    this.mouseExitEventHandler.removeEventListener(listener);
    return this;
  }

  /**
   * Adds a MouseExit event for the Label component.
   *
   * @param listener the event listener to be added
   * @return The Label itself
   */
  public Label addRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    this.rightMouseDownEventHandler.addEventListener(listener);
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
    this.rightMouseDownEventHandler.removeEventListener(listener);
    return this;
  }

  /**
   * Sets whether the lines will be wrapped in the label.
   *
   * @param wrap - Specifies whether the lines will be wrapped (false = Not Wrapped, true = Wrapped)
   * @return Returns this
   */
  public Label setWrap(Boolean wrap) {
    if (getBBjControl() != null) {
      try {
        getBBjControl().setLineWrap(wrap);
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
  @ExcludeFromJacocoGeneratedReport
  public Label setHorizontalAlignment(Alignment alignment) {
    setComponentHorizontalAlignment(alignment);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public Alignment getHorizontalAlignment() {
    return getComponentHorizontalAlignment();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Label setText(String text) {
    super.setText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Label setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Label setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Label setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Label setProperty(String property, Object value) {
    super.setProperty(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Label setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Label addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Label removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }

  private void eventCatchUp() {
    this.mouseEnterEventHandler.catchUp();
    this.mouseExitEventHandler.catchUp();
    this.rightMouseDownEventHandler.catchUp();
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
  }

  private BBjStaticText getBBjControl() {
    try {
      return (BBjStaticText) ComponentAccessor.getDefault().getBBjControl(this);
    } catch (IllegalAccessException e) {
      throw new DwcjRuntimeException(e);
    }
  }
}
