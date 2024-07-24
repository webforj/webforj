package com.webforj.router.annotation;

import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotates a class to define a component as a route in the application.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface Route {
  /**
   * Sets the path of the route.
   *
   * <p>
   * The path is used to navigate to the route. The path should be unique and should not contain any
   * special characters.
   * </p>
   *
   * @return the path of the route
   */
  String value();

  /**
   * Sets the target of the route.
   *
   * <p>
   * If no target is set. The router will render the route's component directly in the first frame
   * of the application.
   * </p>
   *
   * @return the target of the route
   */
  Class<? extends Component> target() default Frame.class;

  /**
   * Sets the frame ID.
   *
   * <p>
   * When the target is set to `Frame.class` the frame ID is used to specify the frame to render the
   * route's component in. if not set, then the first usable frame will be used.
   * </p>
   *
   * @return the frame ID
   */
  String frame() default "";
}
