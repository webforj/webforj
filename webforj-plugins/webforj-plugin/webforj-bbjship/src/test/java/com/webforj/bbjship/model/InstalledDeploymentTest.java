package com.webforj.bbjship.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.bbjship.ApplicationConfiguration;
import com.webforj.plugin.foundation.bbj.HelloApp;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

final class InstalledDeploymentTest {

  @TempDir
  Path temporary;

  @Test
  void shouldCarryEveryValue() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path jar = HelloApp.writeJar(app);
    ApplicationConfiguration configuration = ApplicationConfiguration.load(jar);

    InstalledDeployment deployment = InstalledDeployment.Builder.create().setDirectory(temporary)
        .setClasspath(List.of(jar)).setConfiguration(configuration).build();

    assertEquals(temporary, deployment.getDirectory());
    assertEquals(List.of(jar), deployment.getClasspath());
    assertSame(configuration, deployment.getConfiguration());
  }

  @Test
  void shouldDefaultToEmptyClasspath() {
    InstalledDeployment deployment = InstalledDeployment.Builder.create().build();

    assertNull(deployment.getDirectory());
    assertEquals(List.of(), deployment.getClasspath());
    assertNull(deployment.getConfiguration());
  }

  @Test
  void shouldCarryTheClassFolderOfDevelopmentRun() {
    Path classFolder = temporary.resolve("hello-app/target/classes");

    InstalledDeployment deployment =
        InstalledDeployment.Builder.create().setClassFolders(List.of(classFolder)).build();

    assertEquals(List.of(classFolder), deployment.getClassFolders());
  }

  @Test
  void shouldCarryEveryClassFolderOfDevelopmentRunInOrder() {
    Path classes = temporary.resolve("hello-app/build/classes/java/main");
    Path resources = temporary.resolve("hello-app/build/resources/main");
    List<Path> classFolders = new ArrayList<>(List.of(classes, resources));

    InstalledDeployment deployment =
        InstalledDeployment.Builder.create().setClassFolders(classFolders).build();
    classFolders.clear();

    assertEquals(List.of(classes, resources), deployment.getClassFolders());
  }

  @Test
  void shouldHaveNoClassFolderForArchiveDeployment() {
    InstalledDeployment deployment = InstalledDeployment.Builder.create().build();

    assertEquals(List.of(), deployment.getClassFolders());
  }

  @Test
  void shouldKeepItsValuesAfterTheSuppliedCollectionsChange() {
    List<Path> classpath = new ArrayList<>(List.of(temporary));

    InstalledDeployment deployment =
        InstalledDeployment.Builder.create().setClasspath(classpath).build();
    classpath.clear();

    assertEquals(List.of(temporary), deployment.getClasspath());
    List<Path> storedClasspath = deployment.getClasspath();
    assertThrows(UnsupportedOperationException.class, storedClasspath::clear);
  }

  @Test
  void shouldRefuseNullClassFolders() {
    InstalledDeployment.Builder builder = InstalledDeployment.Builder.create();
    List<Path> none = null;

    assertThrows(NullPointerException.class, () -> builder.setClassFolders(none));
  }

  @Test
  void shouldRejectNullClasspath() {
    InstalledDeployment.Builder withoutClasspath =
        InstalledDeployment.Builder.create().setClasspath(null);

    assertThrows(NullPointerException.class, withoutClasspath::build);
  }
}
