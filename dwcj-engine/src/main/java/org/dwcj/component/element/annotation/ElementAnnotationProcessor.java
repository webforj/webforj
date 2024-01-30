package org.dwcj.component.element.annotation;

import org.dwcj.component.element.ElementComposite;
import org.dwcj.component.element.event.ElementEventOptions;
import org.dwcj.component.event.ComponentEvent;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * Annotation processor for element annotations.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public final class ElementAnnotationProcessor {

  // private constructor to hide the implicit public one
  private ElementAnnotationProcessor() {}

  /**
   * Gets the node name from the class annotation.
   *
   * @see NodeName
   *
   * @return the node name
   * @throws DwcjRuntimeException if the class is not annotated with @NodeName
   */
  public static String processNodeName(Class<? extends ElementComposite> clazz) {
    if (clazz.isAnnotationPresent(NodeName.class)) {
      return clazz.getAnnotation(NodeName.class).value();
    } else {
      throw new DwcjRuntimeException(
          "The component class '" + clazz.getSimpleName() + "' is not annotated with @NodeName");
    }
  }

  /**
   * Gets the event name from the given event class.
   *
   * @see EventName
   *
   * @param eventClass the event class
   * @return The event name
   *
   * @throws DwcjRuntimeException if the event class is not annotated with {@link EventName} or if
   *         {@link EventName#value()} is null or empty
   */
  public static String processEventName(Class<?> eventClass) {
    String eventName = null;

    if (eventClass.isAnnotationPresent(EventName.class)) {
      eventName = eventClass.getAnnotation(EventName.class).value();

      if (eventName == null || eventName.isBlank()) {
        throw new DwcjRuntimeException("The event class '" + eventClass.getName()
            + "' is annotated with @EventName but the " + "value is null or empty");
      }
    } else {
      throw new DwcjRuntimeException(
          "The event class '" + eventClass.getName() + "' is not annotated with @EventName");
    }

    return eventName;
  }

  /**
   * Gets the event options from the given class.
   *
   * @see EventOptions
   *
   * @param clazz the class to get the event options from
   * @return The event options
   */
  public static ElementEventOptions processEventOptions(Class<?> clazz) {
    EventOptions annotation = clazz.getAnnotation(EventOptions.class);
    ElementEventOptions options = new ElementEventOptions();

    if (annotation != null) {
      // Set code and filter
      options.setCode(annotation.code());
      options.setFilter(annotation.filter());

      // Add the data
      for (EventOptions.EventData dataItem : annotation.data()) {
        options.addData(dataItem.key(), dataItem.exp());
      }

      // Set debounce
      if (annotation.debounce().value() >= 0) {
        options.setDebounce(annotation.debounce().value(), annotation.debounce().phase());
      }

      // Set throttle
      if (annotation.throttle() >= 0) {
        options.setThrottle(annotation.throttle());
      }
    }

    return options;
  }
}
