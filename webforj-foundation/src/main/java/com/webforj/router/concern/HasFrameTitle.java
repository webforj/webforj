package com.webforj.router.concern;

import com.webforj.router.NavigationContext;
import com.webforj.router.history.ParametersBag;

/**
 * Allow components to provide a title for the active frame dynamically.
 *
 * <p>
 * The the {@code getFrameTitle} is invoked when before the browser's title is updated but after the
 * navigation target is resolved.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
@FunctionalInterface
public interface HasFrameTitle {

  /**
   * Gets the title of this navigation target.
   *
   * @param context the navigation context
   * @param parameters the route parameters bag
   *
   * @return the title of this navigation target
   */
  String getFrameTitle(NavigationContext context, ParametersBag parameters);
}
