package com.webforj.jbang;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class JbangRestartDisablerTest {

  @BeforeEach
  void clearProperty() {
    System.clearProperty(JbangRestartDisabler.RESTART_ENABLED_PROPERTY);
  }

  @AfterEach
  void restoreProperty() {
    System.clearProperty(JbangRestartDisabler.RESTART_ENABLED_PROPERTY);
  }

  @Test
  void shouldDisableRestartOnConstruction() {
    new JbangRestartDisabler(null, new String[0]);

    assertEquals("false", System.getProperty(JbangRestartDisabler.RESTART_ENABLED_PROPERTY));
  }

  @Test
  void shouldOverrideEnabledRestart() {
    System.setProperty(JbangRestartDisabler.RESTART_ENABLED_PROPERTY, "true");

    new JbangRestartDisabler(null, new String[0]);

    assertEquals("false", System.getProperty(JbangRestartDisabler.RESTART_ENABLED_PROPERTY));
  }
}
