package com.webforj.bbjship.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.Test;

final class PublishedApplicationTest {

  @Test
  void shouldCarryTheNameUrlAndDirectory() {
    Path directory = Path.of("deployments", "11111111-1111-1111-1111-111111111111");

    PublishedApplication application =
        new PublishedApplication("sample", "/webapp/sample", directory, null);

    assertEquals("sample", application.getName());
    assertEquals("/webapp/sample", application.getUrl());
    assertEquals(directory, application.getDirectory());
  }

  @Test
  void shouldCarryTheClassFolderOfDevelopmentRun() {
    Path directory = Path.of("deployments", "11111111-1111-1111-1111-111111111111");
    Path classFolder = Path.of("/work/sample/target/classes");

    PublishedApplication application =
        new PublishedApplication("sample", "/webapp/sample", directory, List.of(classFolder));

    assertEquals(List.of(classFolder), application.getClassFolders());
  }

  @Test
  void shouldCarryEveryClassFolderOfDevelopmentRunInOrder() {
    Path directory = Path.of("deployments", "11111111-1111-1111-1111-111111111111");
    Path classes = Path.of("/work/sample/build/classes/java/main");
    Path resources = Path.of("/work/sample/build/resources/main");

    PublishedApplication application = new PublishedApplication("sample", "/webapp/sample",
        directory, List.of(classes, resources));

    assertEquals(List.of(classes, resources), application.getClassFolders());
  }

  @Test
  void shouldKeepMissingValues() {
    PublishedApplication application = new PublishedApplication(null, null, null, null);

    assertNull(application.getName());
    assertNull(application.getUrl());
    assertNull(application.getDirectory());
    assertEquals(List.of(), application.getClassFolders());
  }
}
