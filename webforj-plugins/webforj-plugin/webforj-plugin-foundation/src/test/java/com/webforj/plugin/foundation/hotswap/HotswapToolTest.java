package com.webforj.plugin.foundation.hotswap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.EnumSet;
import java.util.Optional;
import java.util.Set;
import org.junit.jupiter.api.Test;

class HotswapToolTest {

  @Test
  void shouldStayOffWithoutAnyConfiguration() {
    assertTrue(HotswapTool.select(Set.of(), null).isEmpty());
  }

  @Test
  void shouldPickTheConfiguredTool() {
    Optional<HotswapTool> selected = HotswapTool.select(EnumSet.of(HotswapTool.JREBEL), null);

    assertEquals(Optional.of(HotswapTool.JREBEL), selected);
  }

  @Test
  void shouldLetTheCommandLineEnableTheToolWithoutConfiguration() {
    Optional<HotswapTool> selected = HotswapTool.select(Set.of(), "jrebel");

    assertEquals(Optional.of(HotswapTool.JREBEL), selected);
  }

  @Test
  void shouldLetTheCommandLineSwitchHotswapOff() {
    assertTrue(HotswapTool.select(EnumSet.of(HotswapTool.JREBEL), "off").isEmpty());
  }

  @Test
  void shouldLetTheCommandLineEnableTheHotswapAgent() {
    Optional<HotswapTool> selected = HotswapTool.select(Set.of(), "hotswapAgent");

    assertEquals(Optional.of(HotswapTool.HOTSWAP_AGENT), selected);
  }

  @Test
  void shouldFailWhenTheBuildNamesBothTools() {
    IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
        () -> HotswapTool.select(EnumSet.of(HotswapTool.HOTSWAP_AGENT, HotswapTool.JREBEL), null));

    assertTrue(failure.getMessage().contains("hotswapAgent and jrebel"));
  }

  @Test
  void shouldRejectAnUnknownCommandLineValue() {
    IllegalArgumentException failure =
        assertThrows(IllegalArgumentException.class, () -> HotswapTool.select(Set.of(), "dcevm"));

    assertTrue(failure.getMessage().contains("dcevm"));
    assertTrue(failure.getMessage().contains("jrebel"));
    assertTrue(failure.getMessage().contains("off"));
  }
}
