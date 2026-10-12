package com.webforj.plugin.foundation.bbj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class BBjShipVersionFileTest {

  @TempDir
  Path temporary;

  @Test
  void shouldReadTheVersionOfTheBbjShipPlugin() throws IOException {
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));

    String version = BBjShipVersionFile.readVersion(plugin);

    assertEquals("26.03", version);
  }

  @ParameterizedTest
  @ValueSource(strings = {"version=\n", "version=  \n", "name=bbjship\n"})
  void shouldRefuseVersionFileWithoutVersion(String content) throws IOException {
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Files.writeString(plugin.resolve("version.properties"), content);

    IOException failure =
        assertThrows(IOException.class, () -> BBjShipVersionFile.readVersion(plugin));

    assertEquals("BBjShip version is missing in version.properties", failure.getMessage());
  }

  @Test
  void shouldNameTheVersionFileWhenItIsMissing() throws IOException {
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Files.delete(plugin.resolve("version.properties"));

    NoSuchFileException failure =
        assertThrows(NoSuchFileException.class, () -> BBjShipVersionFile.readVersion(plugin));

    assertEquals(plugin.resolve("version.properties").toString(), failure.getMessage());
  }
}
