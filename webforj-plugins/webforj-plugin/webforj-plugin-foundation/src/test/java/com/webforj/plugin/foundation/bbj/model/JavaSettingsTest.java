package com.webforj.plugin.foundation.bbj.model;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.nio.file.Path;
import java.util.Optional;
import org.junit.jupiter.api.Test;

class JavaSettingsTest {

  @Test
  void shouldHoldTheJavaSettings() {
    JavaSettings settings =
        new JavaSettings(Path.of("/opt/bbx/cfg/BBj.properties"), Path.of("/opt/hotswap"));

    assertEquals(Optional.of(Path.of("/opt/bbx/cfg/BBj.properties")), settings.getPropertiesFile());
    assertEquals(Path.of("/opt/hotswap"), settings.getOptionsDirectory());
  }

  @Test
  void shouldAnswerNoPropertiesFileWhenTheHomeFileApplies() {
    JavaSettings settings = new JavaSettings(null, Path.of("/opt/hotswap"));

    assertEquals(Optional.empty(), settings.getPropertiesFile());
  }
}
