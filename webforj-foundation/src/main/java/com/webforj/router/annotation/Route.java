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

  String AUTO_GENERATED_VIEW_NAME = "___AUTO_GENERATED_VIEW_NAME___";

  /**
   * Sets the path of the route.
   *
   * <p>
   * The path is used to navigate to the route. The path should be unique and should not contain any
   * special characters. If no path is set, the router will generate a path based on the route's
   * component class name.
   * </p>
   *
   * @return the path of the route
   */
  String value() default AUTO_GENERATED_VIEW_NAME;

  /**
   * Sets the target of the route.
   *
   * <p>
   * The target is the place where the route's component will be rendered. The target can be any
   * component that extends the {@link Component} class. The default target is the {@link Frame}.
   * </p>
   *
   * <p>
   * When frame is used as the target, the route's component will be rendered in the frame that can
   * be specified using the {@link #frame()} ID. If no frame ID is set, then the first usable frame
   * will be detected and used to render the route's component. If no frame is found, then the
   * route's component will not be rendered and fail.
   * </p>
   *
   * @return the target of the route
   */
  Class<? extends Component> target() default Frame.class;

  /**
   * Sets the frame ID.
   *
   * <p>
   * When the {@link #target()} is set to {@code Frame.class}, the frame ID can be defined using
   * {@link Frame#setFrameId(java.lang.String)} to specify the frame to render the route's component
   * in. If no frame ID is set, then the first usable frame will be detected and used to render the
   * route's component. If no frame is found, then the route's component will not be rendered and
   * fail.
   * </p>
   *
   * @return the frame ID
   */
  String frame() default "";
}
