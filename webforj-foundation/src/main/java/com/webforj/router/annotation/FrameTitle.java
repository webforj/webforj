package com.webforj.router.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * An annotation to set the frame title of the navigation target.
 *
 * <p>
 * The annotation can be used on the component class level to set the frame title of the navigation
 * target.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
public @interface FrameTitle {
  /**
   * The title of the frame.
   *
   * @return the title of the frame
   */
  String value();
}
