package com.webforj.component.element.annotation;

import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.event.ElementEventOptions;
import com.webforj.exceptions.WebforjRuntimeException;

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
   * @throws WebforjRuntimeException if the class is not annotated with @NodeName
   */
  public static String processNodeName(Class<? extends ElementComposite> clazz) {
    if (clazz.isAnnotationPresent(NodeName.class)) {
      return clazz.getAnnotation(NodeName.class).value();
    } else {
      throw new WebforjRuntimeException(
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
   * @throws WebforjRuntimeException if the event class is not annotated with {@link EventName} or
   *         if {@link EventName#value()} is null or empty
   */
  public static String processEventName(Class<?> eventClass) {
    String eventName = null;

    if (eventClass.isAnnotationPresent(EventName.class)) {
      eventName = eventClass.getAnnotation(EventName.class).value();

      if (eventName == null || eventName.isBlank()) {
        throw new WebforjRuntimeException("The event class '" + eventClass.getName()
            + "' is annotated with @EventName but the " + "value is null or empty");
      }
    } else {
      throw new WebforjRuntimeException(
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
    return EventOptionsAnnotationProcessor.processEventOptions(clazz, new ElementEventOptions());
  }
}
