package com.webforj.component.element.annotation;

import com.webforj.component.element.event.ElementEventOptions;

/**
 * Annotation processor for event options annotations.
 *
 * @see EventOptions
 * @see ElementAnnotationProcessor
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public final class EventOptionsAnnotationProcessor {

  // private constructor to hide the implicit public one
  private EventOptionsAnnotationProcessor() {}

  /**
   * A generic method to process event options with a specified instance.
   *
   * @param <T> the type of ElementEventOptions or its subclass
   * @param clazz the class to get the event options from
   * @param optionsInstance an instance of T where T extends ElementEventOptions
   *
   * @return The initialized options instance of type T
   */
  public static <T extends ElementEventOptions> T processEventOptions(Class<?> clazz,
      T optionsInstance) {
    EventOptions annotation = clazz.getAnnotation(EventOptions.class);

    if (annotation != null) {
      // Set code and filter
      optionsInstance.setCode(annotation.code());
      optionsInstance.setFilter(annotation.filter());

      // Add the data
      for (EventOptions.EventData dataItem : annotation.data()) {
        optionsInstance.addData(dataItem.key(), dataItem.exp());
      }

      // Set debounce
      if (annotation.debounce().value() >= 0) {
        optionsInstance.setDebounce(annotation.debounce().value(), annotation.debounce().phase());
      }

      // Set throttle
      if (annotation.throttle() >= 0) {
        optionsInstance.setThrottle(annotation.throttle());
      }
    }

    return optionsInstance;
  }
}
