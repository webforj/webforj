package com.webforj.testbed.fixture;

import com.webforj.App;
import com.webforj.annotation.Routify;

/** Provides a routed application for lifecycle tests. */
@Routify(packages = "com.webforj.testbed.fixture")
public class TestApp extends App {
  private boolean ran = false;

  @Override
  public void run() {
    ran = true;
  }

  public boolean hasRun() {
    return ran;
  }
}
