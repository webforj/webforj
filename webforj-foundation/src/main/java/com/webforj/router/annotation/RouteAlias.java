package com.webforj.router.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotates a class to define an alias for a route in the application. This should be used in
 * conjunction with a primary {@link Route} annotation.
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(RouteAlias.Container.class)
@Documented
public @interface RouteAlias {


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
  String value();

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

  /**
   * A container for multiple {@link RouteAlias} annotations.
   *
   * @see RouteAlias
   * @author Hyyan Abo Fakher
   */
  @Target(ElementType.TYPE)
  @Retention(RetentionPolicy.RUNTIME)
  @Documented
  public @interface Container {
    /**
     * The array of {@link RouteAlias} annotations.
     *
     * @return the array of {@link RouteAlias} annotations
     */
    RouteAlias[] value();
  }
}
