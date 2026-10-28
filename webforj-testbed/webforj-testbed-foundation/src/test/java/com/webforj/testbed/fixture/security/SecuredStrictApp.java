package com.webforj.testbed.fixture.security;

import com.webforj.App;
import com.webforj.annotation.Routify;

/** Provides a secured application for tests without redirects. */
@Routify(packages = "com.webforj.testbed.fixture.security")
public class SecuredStrictApp extends App {

  @Override
  public void run() {} // NOSONAR
}
