package com.webforj.plugin.maven.bbj.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

class BBjAdminOptionsTest {

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

    options.setUsername("administrator");
    options.setPassword("secret");
    options.setToken("4f2a9c");

    assertEquals("administrator", options.getUsername());
    assertEquals("secret", options.getPassword());
    assertEquals("4f2a9c", options.getToken());
  }

  @Test
  void shouldRejectNullAndKeepTheConfiguredValues() {
    BBjAdminOptions options = new BBjAdminOptions();
    options.setUsername("administrator");
    options.setPassword("secret");
    options.setToken("4f2a9c");
    String missing = null;

    assertThrows(NullPointerException.class, () -> options.setUsername(missing));
    assertThrows(NullPointerException.class, () -> options.setPassword(missing));
    assertThrows(NullPointerException.class, () -> options.setToken(missing));

    assertEquals("administrator", options.getUsername());
    assertEquals("secret", options.getPassword());
    assertEquals("4f2a9c", options.getToken());
  }
}
