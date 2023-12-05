package org.dwcj.component.optioninput.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.sysgui.BBjRadioGroup;
import com.basis.startup.type.BBjException;
import java.lang.reflect.Field;
import java.util.UUID;
import org.dwcj.Environment;
import org.dwcj.bridge.IDwcjBBjBridge;
import org.dwcj.component.Component;
import org.dwcj.component.event.sink.DwcEventSink;
import org.dwcj.component.optioninput.RadioButtonGroup;
import org.dwcj.dispatcher.EventDispatcher;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * The AbstractRadioButtonEventSink sink implements the required logic for setting and removing the
 * callback on a BBjRadioGroup. Subclasses must implement the handleEvent method which is
 * responsible for delegating the BBj event to the corresponding event listener to the Java
 * component.
 *
 * @author Hyyan Abo Fakher
 * @since 23.01
 */
public abstract class AbstractRadioButtonEventSink implements DwcEventSink {

  private final RadioButtonGroup component;
  private EventDispatcher dispatcher;
  private final int eventType;
  private BBjRadioGroup control = null;
  private IDwcjBBjBridge dwcjHelper;

  /**
   * Constructor for the sink class.
   *
   * @param component The Java component
   * @param dispatcher The events dispatcher
   * @param eventType The type of the BBj event
   */
  protected AbstractRadioButtonEventSink(RadioButtonGroup component, EventDispatcher dispatcher,
      int eventType) {
    this.component = component;
    this.dispatcher = dispatcher;
    this.eventType = eventType;

    if (Environment.getCurrent() != null) {
      setDwcjHelper(Environment.getCurrent().getDwcjHelper());
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String setCallback(Object options) {
    if (isConnected()) {
      try {
        getBBjRadioGroup().setCallback(eventType,
            getDwcjHelper().getEventProxy(this, "handleEvent"), "onEvent");
      } catch (BBjException e) {
        throw new DwcjRuntimeException("Failed to set BBjRadioGroup callback.", e);
      }
    }

    return UUID.randomUUID().toString();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeCallback(String callbackId) {
    if (isConnected()) {
      try {
        getBBjRadioGroup().clearCallback(eventType);
      } catch (BBjException e) {
        throw new DwcjRuntimeException("Failed to remove BBjRadioGroup callback.", e);
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final boolean isConnected() {
    return getBBjRadioGroup() != null && getDwcjHelper() != null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final Component getComponent() {
    return this.component;
  }

  /**
   * Handle the BBj event and delegate it to the corresponding event listener to the Java component.
   *
   * @param ev A BBj event
   */
  public abstract void handleEvent(BBjEvent ev);

  /**
   * Set the event dispatcher instance.
   *
   * @param dispatcher the event dispatcher instance.
   */
  void setEventDispatcher(EventDispatcher dispatcher) {
    this.dispatcher = dispatcher;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public EventDispatcher getEventDispatcher() {
    return this.dispatcher;
  }

  /**
   * Set the instance of the DwcjHelper.
   *
   * @param dwcjHelper The DwcjHelper instance.
   */
  void setDwcjHelper(IDwcjBBjBridge dwcjHelper) {
    this.dwcjHelper = dwcjHelper;
  }

  /**
   * Get the instance of the DwcjHelper.
   *
   * @return The DwcjHelper instance.
   */
  IDwcjBBjBridge getDwcjHelper() {
    return this.dwcjHelper;
  }

  /**
   * Get the instance of the underlying BBjControl.
   *
   * @return The BBjControl instance.
   */
  private BBjRadioGroup getBBjRadioGroup() {
    if (control != null) {
      return control;
    }

    try {
      // TODO: implement this using the Accessor Pattern
      Field field = RadioButtonGroup.class.getDeclaredField("group");
      field.setAccessible(true); // NOSONAR
      control = (BBjRadioGroup) field.get(component);
    } catch (IllegalAccessException | NoSuchFieldException | SecurityException e) {
      throw new DwcjRuntimeException("Failed to get original BBjRadioGroup via reflection.", e);
    }

    return control;
  }
}
