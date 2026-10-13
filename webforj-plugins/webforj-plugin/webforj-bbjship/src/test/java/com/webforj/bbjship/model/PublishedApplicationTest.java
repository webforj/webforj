package com.webforj.bbjship.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.Test;

final class PublishedApplicationTest {

  private static final String APP_NAME = "sample";
  private static final String APP_URL = "/webapp/sample";
  private static final String DEPLOYMENTS_FOLDER = "deployments";
  private static final String DEPLOYMENT_ID = "11111111-1111-1111-1111-111111111111";

  @Test
  void shouldCarryTheNameUrlAndDirectory() {
    Path directory = Path.of(DEPLOYMENTS_FOLDER, DEPLOYMENT_ID);

    PublishedApplication application = new PublishedApplication(APP_NAME, APP_URL, directory, null);

    assertEquals(APP_NAME, application.getName());
    assertEquals(APP_URL, application.getUrl());
    assertEquals(directory, application.getDirectory());
  }

  @Test
  void shouldCarryTheClassFolderOfDevelopmentRun() {
    Path directory = Path.of(DEPLOYMENTS_FOLDER, DEPLOYMENT_ID);
    Path classFolder = Path.of("/work/sample/target/classes");

    PublishedApplication application =
        new PublishedApplication(APP_NAME, APP_URL, directory, List.of(classFolder));

    assertEquals(List.of(classFolder), application.getClassFolders());
  }

  @Test
  void shouldCarryEveryClassFolderOfDevelopmentRunInOrder() {
    Path directory = Path.of(DEPLOYMENTS_FOLDER, DEPLOYMENT_ID);
    Path classes = Path.of("/work/sample/build/classes/java/main");
    Path resources = Path.of("/work/sample/build/resources/main");

    PublishedApplication application =
        new PublishedApplication(APP_NAME, APP_URL, directory, List.of(classes, resources));

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
