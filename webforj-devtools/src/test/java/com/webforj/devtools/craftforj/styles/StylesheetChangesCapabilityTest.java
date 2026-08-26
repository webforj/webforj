package com.webforj.devtools.craftforj.styles;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.typesafe.config.ConfigFactory;
import com.webforj.App;
import com.webforj.Environment;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

@DisplayName("StylesheetChangesCapability")
class StylesheetChangesCapabilityTest {

  private final StylesheetChangesCapability capability = new StylesheetChangesCapability();
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
    assertEquals("stylesheetChanges", capability.getKey());
    assertEquals(StylesheetChangesCapability.KEY, capability.getKey());
  }

  @Test
  @DisplayName("Should be supported when the configuration does not mention the key")
  void shouldBeSupportedByDefault() {
    setConfig("");

    assertTrue(capability.isSupported(mock(App.class)));
  }

  @Test
  @DisplayName("Should not be supported when the key is set to false")
  void shouldNotBeSupportedWhenSwitchedOff() {
    setConfig(StylesheetChangesCapability.CONFIG_KEY + " = false");

    assertFalse(capability.isSupported(mock(App.class)));
  }

  @Test
  @DisplayName("Should be supported without an environment on the thread")
  void shouldBeSupportedWithoutEnvironment() {
    environmentMock = mockStatic(Environment.class);
    environmentMock.when(Environment::getCurrent).thenReturn(null);

    assertTrue(capability.isSupported(mock(App.class)));
  }

  private void setConfig(String hocon) {
    Environment environment = mock(Environment.class);
    when(environment.getConfig()).thenReturn(ConfigFactory.parseString(hocon));

    environmentMock = mockStatic(Environment.class);
    environmentMock.when(Environment::getCurrent).thenReturn(environment);
  }
}
