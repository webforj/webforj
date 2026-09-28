package com.webforj;

import com.webforj.bridge.AppAccessor;
import com.webforj.router.history.History;

/**
 * Connects the application accessor to application startup configuration.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class AppAccessorImpl extends AppAccessor {
  /**
   * {@inheritDoc}
   */
  @Override
  public void setRouterHistory(App app, History history) {
    app.setRouterHistory(history);
  }
}
