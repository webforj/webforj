package com.webforj.plugin.maven.bbj.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

class BBjDeploymentOptionsTest {

  private static final String DEPLOYMENT_URL = "https://example.test/webforj-bbjship";

  @Test
  void shouldStartWithoutUrl() {
    BBjDeploymentOptions options = new BBjDeploymentOptions();

    assertEquals("", options.getUrl());
  }

  @Test
  void shouldHoldTheConfiguredUrl() {
    BBjDeploymentOptions options = new BBjDeploymentOptions();

    options.setUrl(DEPLOYMENT_URL);

    assertEquals(DEPLOYMENT_URL, options.getUrl());
  }

  @Test
  void shouldRejectNullAndKeepTheConfiguredUrl() {
    BBjDeploymentOptions options = new BBjDeploymentOptions();
    options.setUrl(DEPLOYMENT_URL);
    String missing = null;

    assertThrows(NullPointerException.class, () -> options.setUrl(missing));

    assertEquals(DEPLOYMENT_URL, options.getUrl());
  }
}
