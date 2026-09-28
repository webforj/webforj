package com.webforj.bridge;

import com.webforj.App;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.router.history.History;
import java.util.Objects;

/**
 * Provides internal framework access to application startup configuration.
 *
 * <p>
 * This bridge follows the friend accessor pattern and is not intended for application code.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public abstract class AppAccessor {
  private static AppAccessor accessor;

  /**
   * Creates an accessor for framework use.
   */
  protected AppAccessor() {}

  /**
   * Returns the accessor registered by {@link App}.
   *
   * @return the application accessor
   */
  public static AppAccessor getDefault() {
    if (accessor == null) {
      try {
        Class.forName(App.class.getName(), true, App.class.getClassLoader());
      } catch (ClassNotFoundException e) {
        throw new WebforjRuntimeException("Unable to load App class.", e);
      }
    }
    if (accessor == null) {
      throw new WebforjRuntimeException("AppAccessor is not initialized.");
    }
    return accessor;
  }

  /**
   * Registers the accessor once during application class initialization.
   *
   * @param value the application accessor
   */
  public static void setDefault(AppAccessor value) {
    if (accessor != null) {
      throw new IllegalStateException("AppAccessor already set and cannot be redefined");
    }
    accessor = Objects.requireNonNull(value, "Accessor must not be null");
  }

  /**
   * Configures the history used when the application constructs its router.
   *
   * @param app the application to configure
   * @param history the history for its router
   * @throws IllegalStateException if application initialization has started
   */
  public abstract void setRouterHistory(App app, History history);
}
