package com.webforj.component.element.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Keeps the property or attribute described by the annotated {@code PropertyDescriptor} field in
 * sync with the client.
 *
 * <p>
 * When one of the given client events fires, the current value of the property or attribute is read
 * on the client and sent to the server with the event payload. The reported value is stored in the
 * server cache so that reading the descriptor returns the value the client last reported instead of
 * the value the server last wrote.
 * </p>
 *
 * <p>
 * Synchronization is not required to read the current client value on demand. The {@code get}
 * method reads the value live from the client when its {@code fromClient} argument is {@code true}.
 * Synchronization is for components that report a value change only through an event, where the
 * reported value rides the event payload rather than a property or attribute the server can read
 * back.
 * </p>
 *
 * <p>
 * The annotation is the declarative form of the {@code synchronize} method in
 * {@code ElementComposite}. Use the method directly when the registration needs to be removed or
 * replaced at runtime.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface Synchronize {

  /**
   * The names of the client events which report the value to the server.
   *
   * @return the event names
   */
  String[] value();

  /**
   * An optional client expression which produces the value when the event fires.
   *
   * <p>
   * When set, the value is read from the given expression instead of the element, for example
   * {@code event.detail}, and the produced value is also written back to the element. Use this form
   * when the element does not maintain the value itself.
   * </p>
   *
   * @return the client expression
   */
  String exp() default "";
}
