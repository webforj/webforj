package com.webforj.bbjship.cli.admin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.bbjship.cli.command.Home;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class BBjShipPluginTest {

  @TempDir
  Path temporary;

  @Test
  void shouldLocateThePluginFilesInsideTheBbjHome() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));

    BBjShipPlugin plugin = new BBjShipPlugin(home.resolve("bin/.."));

    assertEquals(home, plugin.getHome());
    assertEquals(home.resolve("plugins/webforj-bbjship"), plugin.getDirectory());
    assertEquals(home.resolve("plugins/webforj-bbjship/lib/webforj-bbjship.jar"),
        plugin.getLibrary());
    assertEquals(home.resolve(".lib"), plugin.getBBjLibraries());
    assertEquals(home.resolve("cfg/config.bbx"), plugin.getConfigFile());
    assertEquals(home.resolve("plugins/.webforj-bbjship-deployments"), plugin.getRuntime());
    assertEquals(home.resolve("plugins/.webforj-bbjship-deployments/deployments"),
        plugin.getDeployments());
    assertEquals(home.resolve("plugins/.webforj-bbjship-deployments/development.enabled"),
        plugin.getDevelopmentSwitch());
    assertEquals("26.03", plugin.readVersion());
  }

  @Test
  void shouldRefuseVersionFileWithoutVersion() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.writeString(home.resolve("plugins/webforj-bbjship/version.properties"), "name=bbjship\n");
    BBjShipPlugin plugin = new BBjShipPlugin(home);

    IOException failure = assertThrows(IOException.class, plugin::readVersion);

    assertEquals("BBjShip version is missing in version.properties", failure.getMessage());
  }

  @Test
  void shouldRefusePluginFolderThatIsSymbolicLink() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path moved = Files.move(home.resolve("plugins/webforj-bbjship"), temporary.resolve("moved"));
    Files.createSymbolicLink(home.resolve("plugins/webforj-bbjship"), moved);
    BBjShipPlugin plugin = new BBjShipPlugin(home);

    IOException failure = assertThrows(IOException.class, plugin::applyOwnershipValidation);

    assertEquals(
        "The plugin directory is a symbolic link. Install the plugin into a regular directory",
        failure.getMessage());
  }
}
