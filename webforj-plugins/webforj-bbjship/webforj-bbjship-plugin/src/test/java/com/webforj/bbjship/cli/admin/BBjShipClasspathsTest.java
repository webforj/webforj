package com.webforj.bbjship.cli.admin;

import static com.webforj.bbjship.cli.command.BBjServices.CLI_CLASSPATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.webforj.bbjship.AdminList;
import com.webforj.bbjship.cli.command.Home;
import com.webforj.bbjship.foundation.BBjShipPlugin;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InOrder;

class BBjShipClasspathsTest {

  private static final String DEFAULT_CLASSPATH = "(bbj_default)";
  private static final String LIBRARY_JAR = "plugins/webforj-bbjship/lib/webforj-bbjship.jar";
  private static final String BASE_CLASSPATH = "_webforj_bbjship";
  private static final String BBJ_UTIL_JAR = ".lib/BBjUtil.jar";
  private static final String BBJ_JAR = ".lib/BBj.jar";
  private static final String UPDATED_BBJ_UTIL_JAR = ".lib/BBjUtil-26.jar";

  @TempDir
  Path temporary;

  private Path home;
  private BBjAdminBase admin;
  private BBjShipClasspaths classpaths;

  @BeforeEach
  void initServerWithoutBBjShipClasspaths() throws Exception {
    home = Home.createCopy(temporary.resolve("bbx"));
    admin = mock(BBjAdminBase.class);
    when(admin.getClasspathNames()).thenReturn(new AdminList<>(List.of("bbj_default", "addon")));
    classpaths = new BBjShipClasspaths(admin, new BBjShipPlugin(home));
  }

  @Test
  void shouldRegisterTheServletAndBaseClasspaths() throws Exception {
    classpaths.update();

    InOrder calls = inOrder(admin);
    calls.verify(admin).setClasspath(CLI_CLASSPATH,
        List.of(DEFAULT_CLASSPATH, home.resolve(LIBRARY_JAR).toString()));
    calls.verify(admin).setClasspath(BASE_CLASSPATH,
        List.of(home.resolve(BBJ_JAR).toString(), home.resolve(BBJ_UTIL_JAR).toString()));
    calls.verify(admin).commit();
  }

  @ParameterizedTest
  @MethodSource("getOutdatedBaseEntries")
  void shouldUpdateTheBaseClasspathToTheBbjLibraries(List<String> entries) throws Exception {
    String library = home.resolve(LIBRARY_JAR).toString();
    when(admin.getClasspathNames())
        .thenReturn(new AdminList<>(List.of(CLI_CLASSPATH, BASE_CLASSPATH)));
    when(admin.getClasspath(CLI_CLASSPATH))
        .thenReturn(new AdminList<>(List.of(DEFAULT_CLASSPATH, library)));
    when(admin.getClasspath(BASE_CLASSPATH)).thenReturn(
        new AdminList<>(entries.stream().map(entry -> home.resolve(entry).toString()).toList()));

    classpaths.update();

    verify(admin, never()).setClasspath(eq(CLI_CLASSPATH), any());
    InOrder calls = inOrder(admin);
    calls.verify(admin).setClasspath(BASE_CLASSPATH,
        List.of(home.resolve(BBJ_JAR).toString(), home.resolve(BBJ_UTIL_JAR).toString()));
    calls.verify(admin).commit();
  }

  @Test
  void shouldKeepTheClasspathsTheBbjShipOwns() throws Exception {
    String library = home.resolve(LIBRARY_JAR).toString();
    String bbj = home.resolve(BBJ_JAR).toString();
    String util = home.resolve(BBJ_UTIL_JAR).toString();
    when(admin.getClasspathNames())
        .thenReturn(new AdminList<>(List.of(CLI_CLASSPATH, BASE_CLASSPATH)));
    when(admin.getClasspath(CLI_CLASSPATH))
        .thenReturn(new AdminList<>(List.of(DEFAULT_CLASSPATH, library)));
    when(admin.getClasspath(BASE_CLASSPATH)).thenReturn(new AdminList<>(List.of(bbj, util)));

    classpaths.update();

    verify(admin, never()).setClasspath(anyString(), any());
    verify(admin, never()).commit();
  }

  @Test
  void shouldAddOnlyTheMissingBaseClasspath() throws Exception {
    String library = home.resolve(LIBRARY_JAR).toString();
    when(admin.getClasspathNames()).thenReturn(new AdminList<>(List.of(CLI_CLASSPATH)));
    when(admin.getClasspath(CLI_CLASSPATH))
        .thenReturn(new AdminList<>(List.of(DEFAULT_CLASSPATH, library)));

    classpaths.update();

    verify(admin, never()).setClasspath(eq(CLI_CLASSPATH), any());
    InOrder calls = inOrder(admin);
    calls.verify(admin).setClasspath(BASE_CLASSPATH,
        List.of(home.resolve(BBJ_JAR).toString(), home.resolve(BBJ_UTIL_JAR).toString()));
    calls.verify(admin).commit();
  }

  @Test
  void shouldRestoreBothClasspathsWhenTheSecondWriteFails() throws Exception {
    BBjAdminException refused = new BBjAdminException("refused");
    doThrow(refused).when(admin).setClasspath(BASE_CLASSPATH,
        List.of(home.resolve(BBJ_JAR).toString(), home.resolve(BBJ_UTIL_JAR).toString()));

    BBjAdminException failure = assertThrows(BBjAdminException.class, classpaths::update);

    assertSame(refused, failure);
    verify(admin).setClasspath(CLI_CLASSPATH, null);
    verify(admin).setClasspath(BASE_CLASSPATH, null);
  }

  @Test
  void shouldRefuseMissingBbjShipLibrary() throws Exception {
    Files.delete(home.resolve(LIBRARY_JAR));

    IOException failure = assertThrows(IOException.class, classpaths::update);

    assertEquals("The BBjShip library is missing. Check the plugin installation",
        failure.getMessage());
  }

  @Test
  void shouldRefuseAnInstallationWithoutLibraryFolder() throws Exception {
    Files.delete(home.resolve(BBJ_JAR));
    Files.delete(home.resolve(BBJ_UTIL_JAR));
    Files.delete(home.resolve(".lib"));

    IOException failure = assertThrows(IOException.class, classpaths::update);

    assertEquals("Cannot list BBj libraries. Check the BBj library directory",
        failure.getMessage());
  }

  @Test
  void shouldRefuseLibraryFolderWithoutJars() throws Exception {
    Files.delete(home.resolve(BBJ_JAR));
    Files.delete(home.resolve(BBJ_UTIL_JAR));
    Files.writeString(home.resolve(".lib/README.txt"), "libraries");

    IOException failure = assertThrows(IOException.class, classpaths::update);

    assertEquals("No BBj libraries were found. Check the BBj library directory",
        failure.getMessage());
  }

  @Test
  void shouldRefuseServletClasspathOfAnotherApplication() throws Exception {
    when(admin.getClasspathNames()).thenReturn(new AdminList<>(List.of(CLI_CLASSPATH)));
    when(admin.getClasspath(CLI_CLASSPATH))
        .thenReturn(new AdminList<>(List.of(DEFAULT_CLASSPATH, "/srv/other.jar")));

    IOException failure = assertThrows(IOException.class, classpaths::update);

    assertEquals("The BBjShip command classpath belongs to another application",
        failure.getMessage());
    verify(admin, never()).setClasspath(anyString(), any());
  }

  @Test
  void shouldRefuseBaseClasspathWithForeignLibrary() throws Exception {
    Path foreign = Files.writeString(temporary.resolve("foreign.jar"), "jar");
    when(admin.getClasspathNames()).thenReturn(new AdminList<>(List.of(BASE_CLASSPATH)));
    when(admin.getClasspath(BASE_CLASSPATH))
        .thenReturn(new AdminList<>(List.of(foreign.toString())));

    IOException failure = assertThrows(IOException.class, classpaths::update);

    assertEquals("The BBjShip base classpath contains an unowned library", failure.getMessage());
  }

  @Test
  void shouldRefuseBaseEntryThatLeavesTheLibraryFolderThroughALink() throws Exception {
    Path outside = Files.createDirectory(temporary.resolve("outside"));
    Files.createSymbolicLink(home.resolve(".lib/linked"), outside);
    when(admin.getClasspathNames()).thenReturn(new AdminList<>(List.of(BASE_CLASSPATH)));
    when(admin.getClasspath(BASE_CLASSPATH))
        .thenReturn(new AdminList<>(List.of(home.resolve(".lib/linked/../gone.jar").toString())));

    IOException failure = assertThrows(IOException.class, classpaths::removeAll);

    assertEquals("The BBjShip base classpath contains an unowned library", failure.getMessage());
    verify(admin, never()).setClasspath(anyString(), any());
  }

  @Test
  void shouldRestoreReplacedBaseClasspathWhenTheWriteFails() throws Exception {
    String library = home.resolve(LIBRARY_JAR).toString();
    String bbj = home.resolve(BBJ_JAR).toString();
    String util = home.resolve(BBJ_UTIL_JAR).toString();
    when(admin.getClasspathNames())
        .thenReturn(new AdminList<>(List.of(CLI_CLASSPATH, BASE_CLASSPATH)));
    when(admin.getClasspath(CLI_CLASSPATH))
        .thenReturn(new AdminList<>(List.of(DEFAULT_CLASSPATH, library)));
    when(admin.getClasspath(BASE_CLASSPATH)).thenReturn(new AdminList<>(List.of(bbj, util)));
    Files.move(home.resolve(BBJ_UTIL_JAR), home.resolve(UPDATED_BBJ_UTIL_JAR));
    BBjAdminException refused = new BBjAdminException("refused");
    doThrow(refused).when(admin).setClasspath(BASE_CLASSPATH,
        List.of(bbj, home.resolve(UPDATED_BBJ_UTIL_JAR).toString()));

    BBjAdminException failure = assertThrows(BBjAdminException.class, classpaths::update);

    assertSame(refused, failure);
    InOrder calls = inOrder(admin);
    calls.verify(admin).setClasspath(BASE_CLASSPATH, List.of(bbj, util));
    calls.verify(admin).commit();
    verify(admin, never()).setClasspath(BASE_CLASSPATH, null);
  }

  @Test
  void shouldRemoveBaseClasspathNamingLibraryThatNoLongerExists() throws Exception {
    String bbj = home.resolve(BBJ_JAR).toString();
    String util = home.resolve(BBJ_UTIL_JAR).toString();
    when(admin.getClasspathNames()).thenReturn(new AdminList<>(List.of(BASE_CLASSPATH)));
    when(admin.getClasspath(BASE_CLASSPATH)).thenReturn(new AdminList<>(List.of(bbj, util)));
    Files.delete(home.resolve(BBJ_UTIL_JAR));

    classpaths.removeAll();

    InOrder calls = inOrder(admin);
    calls.verify(admin).setClasspath(BASE_CLASSPATH, null);
    calls.verify(admin).commit();
  }

  @ParameterizedTest
  @MethodSource("getUnownedBaseEntries")
  void shouldRefuseRemovingBaseClasspathWithUnownedEntry(String entry) throws Exception {
    when(admin.getClasspathNames()).thenReturn(new AdminList<>(List.of(BASE_CLASSPATH)));
    when(admin.getClasspath(BASE_CLASSPATH))
        .thenReturn(new AdminList<>(List.of(home.resolve(entry).toString())));

    IOException failure = assertThrows(IOException.class, classpaths::removeAll);

    assertEquals("The BBjShip base classpath contains an unowned library", failure.getMessage());
    verify(admin, never()).setClasspath(anyString(), any());
  }

  private static Stream<List<String>> getOutdatedBaseEntries() {
    return Stream.of(List.of(BBJ_JAR), List.of(BBJ_JAR, ".lib/BBjUtil-25.jar"));
  }

  private static Stream<String> getUnownedBaseEntries() {
    return Stream.of(".lib/*", ".lib/BBj?.jar", ".lib/README.txt", "cfg/gone.jar", "gone/gone.jar",
        "/");
  }
}
