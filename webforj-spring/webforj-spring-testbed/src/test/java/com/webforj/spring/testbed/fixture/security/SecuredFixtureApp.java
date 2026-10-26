package com.webforj.spring.testbed.fixture.security;

import com.webforj.App;
import com.webforj.annotation.Routify;

/** Provides a routed application for Spring security tests. */
@Routify(packages = "com.webforj.spring.testbed.fixture.security")
public class SecuredFixtureApp extends App {

  @Override
  public void run() {
    // the routed pages build the application, run has nothing to start
  }
}
