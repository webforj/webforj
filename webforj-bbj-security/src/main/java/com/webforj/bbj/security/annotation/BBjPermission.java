package com.webforj.bbj.security.annotation;

import com.webforj.bbj.security.model.BBjPermissions;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Requires a BBj permission for a route.
 *
 * <p>
 * A user who has not signed in is asked to authenticate. A signed in user who holds none of the
 * listed permissions is denied access. A route that lists no permission is denied to every user.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface BBjPermission {

  /**
   * Gets the permissions of which a user must hold at least one.
   *
   * @return the permissions
   */
  BBjPermissions[] value();
}
