package com.webforj.bbjship.foundation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class BBjShipPluginTest {

  private static final String PLUGIN_PATH = "plugins/webforj-bbjship";

  @TempDir
  Path temporary;

  @Test
  void shouldLocateThePluginFilesInsideTheBbjHome() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    BBjShipPackage.createCopy(home.resolve(PLUGIN_PATH));

    BBjShipPlugin plugin = new BBjShipPlugin(home.resolve("bin/.."));

    assertEquals(home, plugin.getHome());
    assertEquals(home.resolve(PLUGIN_PATH), plugin.getDirectory());
    assertEquals(home.resolve("plugins/webforj-bbjship/lib/webforj-bbjship.jar"),
        plugin.getLibrary());
    assertEquals(home.resolve(".lib"), plugin.getBBjLibraries());
    assertEquals(home.resolve("cfg/config.bbx"), plugin.getConfigFile());
    assertEquals(home.resolve("plugins/.webforj-bbjship-deployments"), plugin.getRuntime());
    assertEquals(home.resolve("plugins/.webforj-bbjship-deployments/deployments"),
        plugin.getDeployments());
    assertEquals(home.resolve("plugins/.webforj-bbjship-deployments/htdocs"),
        plugin.getStaticFiles());
    assertEquals(home.resolve("plugins/.webforj-bbjship-deployments/assets"), plugin.getAssets());
    assertEquals(home.resolve("plugins/.webforj-bbjship-deployments/development.enabled"),
        plugin.getDevelopmentSwitch());
    assertEquals(home.resolve("plugins/.webforj-bbjship-deployments/unencrypted.enabled"),
        plugin.getUnencryptedSwitch());
    assertEquals("26.03", plugin.readVersion());
  }

  @Test
  void shouldLocateTheBbjHomeOfTheSystemProperty() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    System.setProperty("basis.BBjHome", home.toString());
    BBjShipPlugin plugin;
    try {
      plugin = BBjShipPlugin.createFromBBjHome();
    } finally {
      System.clearProperty("basis.BBjHome");
    }

    assertEquals(home, plugin.getHome());
  }

  @Test
  void shouldRefuseVersionFileWithoutVersion() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    BBjShipPackage.createCopy(home.resolve(PLUGIN_PATH));
    Files.writeString(home.resolve("plugins/webforj-bbjship/version.properties"), "name=bbjship\n");
    BBjShipPlugin plugin = new BBjShipPlugin(home);

    IOException failure = assertThrows(IOException.class, plugin::readVersion);

    assertEquals("BBjShip version is missing in version.properties", failure.getMessage());
  }

  @Test
  void shouldRefusePluginFolderThatIsSymbolicLink() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path moved = BBjShipPackage.createCopy(temporary.resolve("moved"));
    Files.createDirectories(home.resolve("plugins"));
    Files.createSymbolicLink(home.resolve(PLUGIN_PATH), moved);
    BBjShipPlugin plugin = new BBjShipPlugin(home);

    IOException failure = assertThrows(IOException.class, plugin::applyOwnershipValidation);

    assertEquals(
        "The plugin directory is a symbolic link. Install the plugin into a regular directory",
        failure.getMessage());
  }
}
