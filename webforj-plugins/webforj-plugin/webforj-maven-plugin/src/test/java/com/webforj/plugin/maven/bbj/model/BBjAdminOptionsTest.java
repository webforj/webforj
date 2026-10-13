package com.webforj.plugin.maven.bbj.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

class BBjAdminOptionsTest {

  private static final String ADMIN_USERNAME = "administrator";
  private static final String ADMIN_PASSWORD = "secret";
  private static final String ADMIN_TOKEN = "4f2a9c";

  @Test
  void shouldStartEmpty() {
    BBjAdminOptions options = new BBjAdminOptions();

    assertNull(options.getUsername());
    assertNull(options.getPassword());
    assertEquals("", options.getToken());
  }

  @Test
  void shouldHoldTheConfiguredValues() {
    BBjAdminOptions options = new BBjAdminOptions();

    options.setUsername(ADMIN_USERNAME);
    options.setPassword(ADMIN_PASSWORD);
    options.setToken(ADMIN_TOKEN);

    assertEquals(ADMIN_USERNAME, options.getUsername());
    assertEquals(ADMIN_PASSWORD, options.getPassword());
    assertEquals(ADMIN_TOKEN, options.getToken());
  }

  @Test
  void shouldRejectNullAndKeepTheConfiguredValues() {
    BBjAdminOptions options = new BBjAdminOptions();
    options.setUsername(ADMIN_USERNAME);
    options.setPassword(ADMIN_PASSWORD);
    options.setToken(ADMIN_TOKEN);
    String missing = null;

    assertThrows(NullPointerException.class, () -> options.setUsername(missing));
    assertThrows(NullPointerException.class, () -> options.setPassword(missing));
    assertThrows(NullPointerException.class, () -> options.setToken(missing));

    assertEquals(ADMIN_USERNAME, options.getUsername());
    assertEquals(ADMIN_PASSWORD, options.getPassword());
    assertEquals(ADMIN_TOKEN, options.getToken());
  }
}
