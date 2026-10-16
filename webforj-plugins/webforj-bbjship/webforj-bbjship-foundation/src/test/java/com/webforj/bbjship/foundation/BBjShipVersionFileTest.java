package com.webforj.bbjship.foundation;

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

  private static final String PLUGIN_NAME = "webforj-bbjship";
  private static final String VERSION_FILE = "version.properties";

  @TempDir
  Path temporary;

  @Test
  void shouldReadTheVersionOfTheBbjShipPlugin() throws IOException {
    Path plugin = BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME));

    String version = BBjShipVersionFile.readVersion(plugin);

    assertEquals("26.03", version);
  }

  @ParameterizedTest
  @ValueSource(strings = {"version=\n", "version=  \n", "name=bbjship\n"})
  void shouldRefuseVersionFileWithoutVersion(String content) throws IOException {
    Path plugin = BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME));
    Files.writeString(plugin.resolve(VERSION_FILE), content);

    IOException failure =
        assertThrows(IOException.class, () -> BBjShipVersionFile.readVersion(plugin));

    assertEquals("BBjShip version is missing in version.properties", failure.getMessage());
  }

  @Test
  void shouldNameTheVersionFileWhenItIsMissing() throws IOException {
    Path plugin = BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME));
    Files.delete(plugin.resolve(VERSION_FILE));

    NoSuchFileException failure =
        assertThrows(NoSuchFileException.class, () -> BBjShipVersionFile.readVersion(plugin));

    assertEquals(plugin.resolve(VERSION_FILE).toString(), failure.getMessage());
  }
}
