package com.webforj.matchmedia;

import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.Environment;

/**
 * Releases media queries before the application's page is destroyed.
 *
 * @since 26.03
 */
public final class MediaQueryLifecycleListener implements AppLifecycleListener {

  @Override
  public void onWillTerminate(App app) {
    if (Environment.isPresent()) {
      MediaQuery.ifPresent(MediaQuery::destroy);
    }
  }
}
