package com.webforj.router.annotation;

import com.webforj.ViewTransition;
import com.webforj.annotation.Experimental;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotates a route component to specify view transition animations.
 *
 * <p>
 * When applied to a route component, the router will automatically animate the component when it
 * enters or exits the view during navigation.
 * </p>
 *
 * <pre>
 * {@literal @}Route
 * {@literal @}RouteTransition(enter = ViewTransition.SLIDE_LEFT, exit = ViewTransition.SLIDE_RIGHT)
 * public class MyView extends Composite&lt;Div&gt; {
 *   // ...
 * }
 * </pre>
 *
 * @author Hyyan Abo Fakher
 * @since 25.11
 *
 * @see ViewTransition
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Experimental(since = "25.11")
public @interface RouteTransition {

  /**
   * The animation type when this component enters the view.
   *
   * @return the enter animation type
   */
  String enter() default ViewTransition.NONE;

  /**
   * The animation type when this component exits the view.
   *
   * @return the exit animation type
   */
  String exit() default ViewTransition.NONE;
}
