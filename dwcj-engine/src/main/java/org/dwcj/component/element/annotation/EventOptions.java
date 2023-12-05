package org.dwcj.component.element.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import org.dwcj.component.element.event.DebouncePhase;

/**
 * Annotation to configure ElementEventOptions.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
public @interface EventOptions {
  /**
   * Represents a key-value pair for event data configuration.
   *
   * @author Hyyan Abo Fakher
   * @since 23.05
   */
  public @interface EventData {

    /** Name of item data to create. */
    String key();

    /**
     * The JavaScript expression that returns item's value, typically
     * <code>event.target.property</code> or <code>component.property</code>.
     */
    String exp();
  }

  /**
   * Configures the event debounce behavior.
   *
   * @param Hyyan Abo Fakher
   * @since 23.06
   */
  public @interface DebounceSettings {

    /** The debounce delay in milliseconds. */
    int value();

    /** The debounce phase. */
    DebouncePhase phase() default DebouncePhase.TRAILING;
  }

  /**
   * The event data as an array of {@link EventData}, representing key-expression pairs.
   *
   * @return The event items to be added.
   */
  EventData[] data() default {};

  /**
   * The JavaScript code to be executed when the event is fired.
   *
   * @return The JavaScript code for the event.
   */
  String code() default "";

  /**
   * The JavaScript filter expression to filter the event in the client.
   *
   * @return The JavaScript filter expression for the event.
   */
  String filter() default "";

  /**
   * The debounce configuration for the event.
   *
   * @return The debounce configuration for the event.
   */
  DebounceSettings debounce() default @DebounceSettings(value = -1, phase = DebouncePhase.TRAILING);

  /**
   * The throttle configuration for the event.
   *
   * @return The throttle configuration for the event.
   */
  int throttle() default -1;
}

