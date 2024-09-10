package com.webforj.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Indicates that an app is routable and provides a set of packages to scan for routes.
 *
 * <p>
 * Classes annotated with this annotation are expected to define routing logic or route
 * configurations for the application. When the app is started, the framework will scan the
 * specified packages for route definitions and process them to create the routing table.
 * </p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Inherited
@Documented
public @interface Routify {

  /**
   * Specifies the packages to be scanned for route definitions.
   *
   * <p>
   * * The {@code packages} element specifies which packages should be scanned to discover routes
   * within the application. These packages should contain route handling classes or definitions
   * that can be processed for route resolution.
   * </p>
   *
   * @return an array of package names to look for routes
   */
  String[] packages() default {};

  /**
   * Specifies whether the framework should initialize the first frame.
   *
   * <p>
   * The {@code initFrame} element specifies whether the framework should initialize the first frame
   * when the app is started. The default value is {@code true}.
   * </p>
   *
   * @return whether the framework should initialize the first frame
   */
  boolean initializeFrame() default true;
}
