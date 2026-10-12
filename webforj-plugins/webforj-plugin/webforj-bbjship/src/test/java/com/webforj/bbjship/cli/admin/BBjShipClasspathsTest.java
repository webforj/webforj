package com.webforj.bbjship.cli.admin;

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
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.webforj.bbjship.cli.command.Home;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.InOrder;

class BBjShipClasspathsTest {

  @TempDir
  Path temporary;

  private Path home;
  private BBjAdminBase admin;
  private BBjShipClasspaths classpaths;

  @BeforeEach
  void initServerWithoutBBjShipClasspaths() throws Exception {
    home = Home.createCopy(temporary.resolve("bbx"));
    admin = mock(BBjAdminBase.class);
    when(admin.getClasspathNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("bbj_default", "addon")));
    classpaths = new BBjShipClasspaths(admin, new BBjShipPlugin(home));
  }

  @Test
  void shouldRegisterTheServletAndBaseClasspaths() throws Exception {
    classpaths.addMissing();

    InOrder calls = inOrder(admin);
    calls.verify(admin).setClasspath("_webforj_bbjship_cli", List.of("(bbj_default)",
        home.resolve("plugins/webforj-bbjship/lib/webforj-bbjship.jar").toString()));
    calls.verify(admin).setClasspath("_webforj_bbjship", List
        .of(home.resolve(".lib/BBj.jar").toString(), home.resolve(".lib/BBjUtil.jar").toString()));
    calls.verify(admin).commit();
  }

  @Test
  void shouldKeepTheClasspathsTheBbjShipOwns() throws Exception {
    String library = home.resolve("plugins/webforj-bbjship/lib/webforj-bbjship.jar").toString();
    String bbj = home.resolve(".lib/BBj.jar").toString();
    when(admin.getClasspathNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("_webforj_bbjship_cli", "_webforj_bbjship")));
    when(admin.getClasspath("_webforj_bbjship_cli"))
        .thenReturn(new BBjAdminAdapterList<>(List.of("(bbj_default)", library)));
    when(admin.getClasspath("_webforj_bbjship"))
        .thenReturn(new BBjAdminAdapterList<>(List.of(bbj)));

    classpaths.addMissing();

    verify(admin, never()).setClasspath(anyString(), any());
    verify(admin, never()).commit();
  }

  @Test
  void shouldAddOnlyTheMissingBaseClasspath() throws Exception {
    String library = home.resolve("plugins/webforj-bbjship/lib/webforj-bbjship.jar").toString();
    when(admin.getClasspathNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("_webforj_bbjship_cli")));
    when(admin.getClasspath("_webforj_bbjship_cli"))
        .thenReturn(new BBjAdminAdapterList<>(List.of("(bbj_default)", library)));

    classpaths.addMissing();

    verify(admin, never()).setClasspath(eq("_webforj_bbjship_cli"), any());
    InOrder calls = inOrder(admin);
    calls.verify(admin).setClasspath("_webforj_bbjship", List
        .of(home.resolve(".lib/BBj.jar").toString(), home.resolve(".lib/BBjUtil.jar").toString()));
    calls.verify(admin).commit();
  }

  @Test
  void shouldRestoreBothClasspathsWhenTheSecondWriteFails() throws Exception {
    BBjAdminException refused = new BBjAdminException("refused");
    doThrow(refused).when(admin).setClasspath("_webforj_bbjship", List
        .of(home.resolve(".lib/BBj.jar").toString(), home.resolve(".lib/BBjUtil.jar").toString()));

    BBjAdminException failure = assertThrows(BBjAdminException.class, classpaths::addMissing);

    assertSame(refused, failure);
    verify(admin).setClasspath("_webforj_bbjship_cli", null);
    verify(admin).setClasspath("_webforj_bbjship", null);
  }

  @Test
  void shouldRefuseMissingBbjShipLibrary() throws Exception {
    Files.delete(home.resolve("plugins/webforj-bbjship/lib/webforj-bbjship.jar"));

    IOException failure = assertThrows(IOException.class, classpaths::addMissing);

    assertEquals("The BBjShip library is missing. Check the plugin installation",
        failure.getMessage());
  }

  @Test
  void shouldRefuseAnInstallationWithoutLibraryFolder() throws Exception {
    Files.delete(home.resolve(".lib/BBj.jar"));
    Files.delete(home.resolve(".lib/BBjUtil.jar"));
    Files.delete(home.resolve(".lib"));

    IOException failure = assertThrows(IOException.class, classpaths::addMissing);

    assertEquals("Cannot list BBj libraries. Check the BBj library directory",
        failure.getMessage());
  }

  @Test
  void shouldRefuseLibraryFolderWithoutJars() throws Exception {
    Files.delete(home.resolve(".lib/BBj.jar"));
    Files.delete(home.resolve(".lib/BBjUtil.jar"));
    Files.writeString(home.resolve(".lib/README.txt"), "libraries");

    IOException failure = assertThrows(IOException.class, classpaths::addMissing);

    assertEquals("No BBj libraries were found. Check the BBj library directory",
        failure.getMessage());
  }

  @Test
  void shouldRefuseServletClasspathOfAnotherApplication() throws Exception {
    when(admin.getClasspathNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("_webforj_bbjship_cli")));
    when(admin.getClasspath("_webforj_bbjship_cli"))
        .thenReturn(new BBjAdminAdapterList<>(List.of("(bbj_default)", "/srv/other.jar")));

    IOException failure = assertThrows(IOException.class, classpaths::addMissing);

    assertEquals("The BBjShip command classpath belongs to another application",
        failure.getMessage());
    verify(admin, never()).setClasspath(anyString(), any());
  }

  @Test
  void shouldRefuseBaseClasspathWithForeignLibrary() throws Exception {
    Path foreign = Files.writeString(temporary.resolve("foreign.jar"), "jar");
    when(admin.getClasspathNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("_webforj_bbjship")));
    when(admin.getClasspath("_webforj_bbjship"))
        .thenReturn(new BBjAdminAdapterList<>(List.of(foreign.toString())));

    IOException failure = assertThrows(IOException.class, classpaths::addMissing);

    assertEquals("The BBjShip base classpath contains an unowned library", failure.getMessage());
  }
}
