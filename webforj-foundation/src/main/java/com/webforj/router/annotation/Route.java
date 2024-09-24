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
 * @since 24.12
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface Route {
  /**
   * Defines the type of the route.
   */
  enum Type {
    /**
     * The route is a layout route. A layout route is a pathless route and used to wrap child routes
     * with additional components and logic, without requiring a matching path in the URL.
     *
     * <p>
     * When set to {@code true}, the route's path will be prefixed with "@" character
     * </p>
     */
    LAYOUT,

    /**
     * The route is a view route. A view route is a route that has a path and can be navigated to.
     * The view route is the default type of the route.
     */
    VIEW,

    /**
     * Automatically determine the type of the route based on the route's component class name.
     *
     * <p>
     * If the route's component class name ends with "Layout", then the route type will be
     * {@link Type#LAYOUT}. Otherwise, the route type will be {@link Type#VIEW}.
     * </p>
     */
    AUTO_DETECT,
  }

  String AUTO_GENERATED_NAME = "___AUTO_GENERATED_NAME___";

  /**
   * Sets the type of the route.
   *
   * <p>
   * The type of the route can be either a {@link Type#VIEW} or a {@link Type#LAYOUT}. The default
   * type is {@link Type#AUTO_DETECT} which will automatically determine the type of the route based
   * on the route's component class name.
   * </p>
   *
   * @return the type of the route
   */
  Type type() default Type.AUTO_DETECT;

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
  String value() default AUTO_GENERATED_NAME;

  /**
   * Sets the outlet of the route.
   *
   * <p>
   * The outlet is the place where the route's component will be rendered. The outlet can be any
   * component that extends the {@link Component} class. The default outlet is the {@link Frame}.
   * </p>
   *
   * <p>
   * When frame is used as the outlet, the route's component will be rendered in the frame that can
   * be specified using the {@link #frame()} ID. If no frame ID is set, then the first usable frame
   * will be detected and used to render the route's component. If no frame is found, then the
   * route's component will not be rendered and fail.
   * </p>
   *
   * @return the target of the route
   */
  Class<? extends Component> outlet() default Frame.class;

  /**
   * Sets the frame name.
   *
   * <p>
   * When the {@link #outlet()} is set to {@code Frame.class}, the frame name can be defined using
   * {@link Frame#setName(java.lang.String)} to specify the frame to render the route's component
   * in. If no frame name is set, then the first usable frame will be detected and used to render
   * the route's component. If no frame is found, then the route's component will not be rendered
   * and fail.
   * </p>
   *
   * @return the frame name
   */
  String frame() default "";

  /**
   * Sets the priority of the route.
   *
   * <p>
   * The priority is used to determine the order of the route when matching the route path. The
   * route with the highest priority will be matched first. If two routes have the same priority,
   * then the route that was registered first will be matched first. The default priority is
   * {@code 10}.
   * </p>
   *
   * @return the priority of the route
   */
  int priority() default 10;
}
