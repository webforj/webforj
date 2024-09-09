package com.webforj.router.observer;

/**
 * Allow components to provide a title for the page dynamically.
 *
 * <p>
 * The the {@code getPageTitle} is invoked when the page is navigated to and the navigation is
 * complete.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
@FunctionalInterface
public interface PageTitleObserver {
  /**
   * Gets the title of this navigation target.
   *
   * @return the title of this navigation target
   */
  String getPageTitle();
}
