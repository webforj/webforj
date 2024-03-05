package com.webforj.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Indicates that the annotated element is experimental and subject to change or removal. This
 * annotation serves as a warning that the element is not yet stable and may be modified in future
 * versions. Use this annotation to inform users of the API about the experimental nature of certain
 * elements within the codebase.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(value = {ElementType.CONSTRUCTOR, ElementType.FIELD, ElementType.LOCAL_VARIABLE,
    ElementType.METHOD, ElementType.PACKAGE, ElementType.MODULE, ElementType.PARAMETER,
    ElementType.TYPE})
public @interface Experimental {

  /**
   * Specifies the version since which the annotated element has been considered experimental. The
   * format of the version string should align with the project's versioning scheme. This can be
   * used to indicate the specific release in which the feature, class, method, etc., was introduced
   * in an experimental state. The default value is an empty string, indicating that no specific
   * version is associated.
   *
   * @return The version string in the format consistent with the project's versioning.
   */
  String since() default "";

  /**
   * Provides a custom warning message to indicate the experimental status of the annotated element.
   * This message is intended to inform or warn users of the API about potential instability or
   * future changes. The default message is a generic warning about the experimental nature of the
   * element.
   *
   * @return The custom warning message.
   */
  String warning() default "This feature is experimental and may change in the future.";
}
