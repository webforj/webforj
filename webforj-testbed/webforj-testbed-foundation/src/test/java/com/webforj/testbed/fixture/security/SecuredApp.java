package com.webforj.testbed.fixture.security;

import com.webforj.App;
import com.webforj.annotation.Routify;

/** Provides a routed application for security tests. */
@Routify(packages = "com.webforj.testbed.fixture.security")
public class SecuredApp extends App {

  @Override
  public void run() {} // NOSONAR
}
