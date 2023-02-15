package org.dwcj.webcomponent.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to define a web component DOM event expressions.
 * 
 * An expression is a string that is evaluated by the javascript engine to
 * determine the value of the expression.
 * 
 * Every expression have access to the following variables:
 * 
 * <ul>
 * <li><b>event</b> - The event object</li>
 * <li><b>component</b> - The web component instance</li>
 * </ul>
 * 
 * When working with expressions keep the following points in mind:
 * <ul>
 *  <li>Expressions are evaluated in the context of the web component instance</li>
 *  <li>If the expression must return a value and has the word return in it, then we will assume it is a multi-line expression and will not wrap it.</li>
 *  <li>If the expression must return a value and does not have the word return in it, then we will insert the return statement and the ";".</li>
 *  <li>If the expression has many lines, then you will need to provide the ";"" at the end of each line and also provide the return statement.</li>
 * </ul>
 * 
 * <a href=
 * "https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault">preventDefault</a>,
 * <a href=
 * "https://developer.mozilla.org/en-US/docs/Web/API/Event/stopPropagation">stopPropagation</a>
 * and
 * <a href=
 * "https://developer.mozilla.org/en-US/docs/Web/API/Event/stopImmediatePropagation">stopImmediatePropagation</a>
 * expressions
 * should return true if the event should be prevented, stopped or stopped
 * immediately. Otherwise, they should return false.
 * 
 * For example, the following expression will prevent the default action of the
 * event if the value of the component's "value" property is equal to "test":
 * 
 * <pre>
 * {@code
 * &#64;EventExpressions(preventDefault = "if (component.value == 'test') return true;")
 * }
 * </pre>
 * 
 * The "detail" expression should set the detail of the event. For example, the
 * following expression will set the detail of the event
 * to the value of the component's "value" property. Then the event detail will
 * available in the event handler.
 * 
 * <pre>
 * {@code
 * &#64;EventExpressions(detail = "event.detail.value = event.target.value;")
 * }
 * </pre>
 * 
 * The "Filter" expression should return true if the event should be fired.
 * Otherwise,
 * it should return false. For example, the following expression will fire the
 * event only if the event target is the same as the component:
 * 
 * <pre>
 * {@code
 * &#64;EventExpressions(filter = "event.target.isSameNode(component)")
 * }
 * </pre>
 * 
 * @author Hyyan Abo Fakher
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
public @interface EventExpressions {

  /** The filter expression to check if the event should be fired. */
  String filter() default "";

  /** The data builder expression to build the event detail. */
  String detail() default "";

  /** The prevent default expression. */
  String preventDefault() default "";

  /** The stop propagation expression. */
  String stopPropagation() default "";

  /** The stop immediate propagation expression. */
  String stopImmediatePropagation() default "";
}
