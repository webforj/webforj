package org.dwcj.component.element.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to configure ElementEventOptions.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.FIELD, ElementType.PARAMETER,
    ElementType.LOCAL_VARIABLE})
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
  public @interface EventMapItem {

    /** Name of item data to create. */
    String key();

    /**
     * The JavaScript expression that returns item's value, typically
     * <code>event.target.property</code> or <code>component.property</code>.
     */
    String exp();
  }

  /**
   * The event data as an array of {@link EventMapItem}, representing key-expression pairs.
   *
   * @return The event items to be added.
   */
  EventMapItem[] map() default {};

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
}

