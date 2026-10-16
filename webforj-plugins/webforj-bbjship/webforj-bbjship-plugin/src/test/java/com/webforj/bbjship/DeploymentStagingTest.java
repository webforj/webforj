package com.webforj.bbjship;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bbjship.foundation.HelloApp;
import com.webforj.bbjship.model.InstalledDeployment;
import com.webforj.bbjship.model.PublishedApplication;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class DeploymentStagingTest {

  private static final String APP_NAME = "hello-app";

  @TempDir
  Path temporary;

  private Path archive;
  private Path root;

  @BeforeEach
  void initHelloAppArchive() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    root = temporary.resolve("deployments");
  }

  @Test
  void shouldWriteTheArchiveIntoNewDeploymentDirectoryOfThePublishName() throws IOException {
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));

    InstalledDeployment deployment =
        DeploymentStaging.writeDeployment(upload, root, APP_NAME, List.of());

    Path directory = deployment.getDirectory();
    assertEquals(root.resolve(APP_NAME), directory.getParent());
    assertTrue(Files.isRegularFile(directory.resolve("bbj/webforj.bbj")));
    assertEquals(directory.resolve("application.jar"),
        deployment.getClasspath().get(deployment.getClasspath().size() - 1));
    assertEquals(List.of(), deployment.getClassFolders());
    try (Stream<Path> files = Files.list(root)) {
      assertEquals(List.of(root.resolve(APP_NAME)), files.toList());
    }
  }

  @Test
  void shouldRefuseTheEntryClassPublishedUnderAnotherNameAndLeaveNoFiles() throws IOException {
    InstalledDeployment published = DeploymentStaging.writeDeployment(
        new ByteArrayInputStream(Files.readAllBytes(archive)), root, APP_NAME, List.of());
    DeploymentFolder.writeCurrent(published.getDirectory());
    List<PublishedApplication> applications = List.of(new PublishedApplication(APP_NAME,
        "/webapp/hello-app", published.getDirectory(), List.of()));
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));

    DeploymentException refusal = assertThrows(DeploymentException.class,
        () -> DeploymentStaging.writeDeployment(upload, root, "other-app", applications));

    assertEquals("Entry class org.example.Application is already published as hello-app. "
        + "Undeploy hello-app first", refusal.getMessage());
    try (Stream<Path> files = Files.list(root)) {
      assertEquals(List.of(root.resolve(APP_NAME)), files.toList());
    }
  }

  @Test
  void shouldRefuseTheArchiveWithoutLauncherAndLeaveNoFiles() throws IOException {
    try (FileSystem zip = FileSystems.newFileSystem(archive)) {
      Files.delete(zip.getPath("bbj/webforj.bbj"));
    }

    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));

    DeploymentException refusal = assertThrows(DeploymentException.class,
        () -> DeploymentStaging.writeDeployment(upload, root, APP_NAME, List.of()));

    assertEquals("BBj deployment launcher is missing", refusal.getMessage());
    try (Stream<Path> files = Files.list(root)) {
      assertEquals(List.of(), files.toList());
    }
  }
}
