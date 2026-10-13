package com.webforj.plugin.foundation.bbj.model;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.nio.file.Path;
import java.util.Optional;
import org.junit.jupiter.api.Test;

class JavaSettingsTest {

  private static final String OPTIONS_DIRECTORY = "/opt/hotswap";

  @Test
  void shouldHoldTheJavaSettings() {
    JavaSettings settings =
        new JavaSettings(Path.of("/opt/bbx/cfg/BBj.properties"), Path.of(OPTIONS_DIRECTORY));

    assertEquals(Optional.of(Path.of("/opt/bbx/cfg/BBj.properties")), settings.getPropertiesFile());
    assertEquals(Path.of(OPTIONS_DIRECTORY), settings.getOptionsDirectory());
  }

  @Test
  void shouldAnswerNoPropertiesFileWhenTheHomeFileApplies() {
    JavaSettings settings = new JavaSettings(null, Path.of(OPTIONS_DIRECTORY));

    assertEquals(Optional.empty(), settings.getPropertiesFile());
  }
}
