package com.webforj.testbed.junit.fixture;

import com.webforj.App;
import com.webforj.annotation.Routify;

/** Provides a routed application for JUnit integration tests. */
@Routify(packages = "com.webforj.testbed.junit.fixture")
public class JunitApp extends App {
  @Override
  public void run() {
    // the routed pages build the application, run has nothing to start
  }
}
