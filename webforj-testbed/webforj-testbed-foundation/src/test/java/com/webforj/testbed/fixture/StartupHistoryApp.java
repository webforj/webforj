package com.webforj.testbed.fixture;

import com.webforj.App;
import com.webforj.annotation.Routify;
import com.webforj.router.Router;

/** Captures the router during application startup for history tests. */
@Routify(packages = "com.webforj.testbed.fixture")
public class StartupHistoryApp extends App {
  private Router startupRouter;

  @Override
  protected void onWillRun() {
    startupRouter = Router.getCurrent();
  }

  @Override
  public void run() {
    startupRouter.navigate(AboutView.class);
  }

  public Router getStartupRouter() {
    return startupRouter;
  }
}
