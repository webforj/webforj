package com.webforj.devtools.craftforj.inspector.source;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.typesafe.config.ConfigFactory;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.devtools.craftforj.capabilities.CraftforjCapability;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

@DisplayName("SourceFreeformChangesCapability")
class SourceFreeformChangesCapabilityTest {

  private MockedStatic<Environment> environmentMock;

  @AfterEach
  void closeEnvironmentMock() {
    if (environmentMock != null) {
      environmentMock.close();
      environmentMock = null;
    }
  }

  @Test
  @DisplayName("Should carry the key the panel receives")
  void shouldCarryPanelKey() {
    CraftforjCapability capability =
        new SourceFreeformChangesCapability(createCheck(true), createCheck(true));

    assertEquals("sourceFreeformChanges", capability.getKey());
    assertEquals(SourceFreeformChangesCapability.KEY, capability.getKey());
  }

  @Test
  @DisplayName("Should be supported when both checks pass and the switch is on")
  void shouldBeSupportedWhenBothChecksPass() {
    setConfig(SourceFreeformChangesCapability.CONFIG_KEY + " = true");

    CraftforjCapability capability =
        new SourceFreeformChangesCapability(createCheck(true), createCheck(true));

    assertTrue(capability.isSupported(mock(App.class)));
  }

  @Test
  @DisplayName("Should not be supported when the source changes check fails")
  void shouldNotBeSupportedWhenSourceChangesFail() {
    setConfig(SourceFreeformChangesCapability.CONFIG_KEY + " = true");

    CraftforjCapability capability =
        new SourceFreeformChangesCapability(createCheck(false), createCheck(true));

    assertFalse(capability.isSupported(mock(App.class)));
  }

  @Test
  @DisplayName("Should not be supported when the assistant check fails")
  void shouldNotBeSupportedWhenAssistantCheckFails() {
    setConfig(SourceFreeformChangesCapability.CONFIG_KEY + " = true");

    CraftforjCapability capability =
        new SourceFreeformChangesCapability(createCheck(true), createCheck(false));

    assertFalse(capability.isSupported(mock(App.class)));
  }

  @Test
  @DisplayName("Should not be supported when the switch is off")
  void shouldNotBeSupportedWhenSwitchedOff() {
    setConfig(SourceFreeformChangesCapability.CONFIG_KEY + " = false");

    CraftforjCapability capability =
        new SourceFreeformChangesCapability(createCheck(true), createCheck(true));

    assertFalse(capability.isSupported(mock(App.class)));
  }

  @Test
  @DisplayName("Should wire the real checks and pass on an empty configuration")
  void shouldWireRealChecksOnEmptyConfiguration() {
    setConfig("");

    assertTrue(new SourceFreeformChangesCapability().isSupported(mock(App.class)));
  }

  private void setConfig(String hocon) {
    Environment environment = mock(Environment.class);
    when(environment.getConfig()).thenReturn(ConfigFactory.parseString(hocon));

    environmentMock = mockStatic(Environment.class);
    environmentMock.when(Environment::getCurrent).thenReturn(environment);
  }

  private static CraftforjCapability createCheck(boolean supported) {
    CraftforjCapability check = mock(CraftforjCapability.class);
    when(check.isSupported(any())).thenReturn(supported);

    return check;
  }
}
