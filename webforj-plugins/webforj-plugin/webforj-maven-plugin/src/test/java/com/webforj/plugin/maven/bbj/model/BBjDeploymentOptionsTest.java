package com.webforj.plugin.maven.bbj.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

class BBjDeploymentOptionsTest {

  @Test
  void shouldStartWithoutUrl() {
    BBjDeploymentOptions options = new BBjDeploymentOptions();

    assertEquals("", options.getUrl());
  }

  @Test
  void shouldHoldTheConfiguredUrl() {
    BBjDeploymentOptions options = new BBjDeploymentOptions();

    options.setUrl("https://example.test/webforj-bbjship");

    assertEquals("https://example.test/webforj-bbjship", options.getUrl());
  }

  @Test
  void shouldRejectNullAndKeepTheConfiguredUrl() {
    BBjDeploymentOptions options = new BBjDeploymentOptions();
    options.setUrl("https://example.test/webforj-bbjship");
    String missing = null;

    assertThrows(NullPointerException.class, () -> options.setUrl(missing));

    assertEquals("https://example.test/webforj-bbjship", options.getUrl());
  }
}
