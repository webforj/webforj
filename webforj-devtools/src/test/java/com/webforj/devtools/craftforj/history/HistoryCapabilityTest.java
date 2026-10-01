package com.webforj.devtools.craftforj.history;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import com.webforj.App;
import com.webforj.devtools.craftforj.capabilities.CraftforjCapability;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("HistoryCapability")
class HistoryCapabilityTest {

  @Test
  @DisplayName("Should announce itself under the history key")
  void shouldAnnounceHistoryKey() {
    assertEquals("history", new HistoryCapability().getKey());
  }

  @Test
  @DisplayName("Should be supported while any write is supported")
  void shouldBeSupportedWithAnyWrite() {
    App app = mock(App.class);

    assertTrue(
        new HistoryCapability(List.of(createWrite(false), createWrite(true))).isSupported(app));
    assertFalse(
        new HistoryCapability(List.of(createWrite(false), createWrite(false))).isSupported(app));
    assertFalse(new HistoryCapability(List.of()).isSupported(app));
  }

  private static CraftforjCapability createWrite(boolean supported) {
    return new CraftforjCapability() {
      @Override
      public String getKey() {
        return "write";
      }

      @Override
      public boolean isSupported(App app) {
        return supported;
      }
    };
  }
}
