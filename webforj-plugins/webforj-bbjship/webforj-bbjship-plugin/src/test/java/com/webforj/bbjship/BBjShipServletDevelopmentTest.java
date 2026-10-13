package com.webforj.bbjship;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.AdditionalAnswers.delegatesTo;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminAppDeploymentApplication;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.google.gson.JsonParser;
import com.webforj.bbjship.foundation.DeploymentArchive;
import com.webforj.bbjship.foundation.HelloApp;
import jakarta.servlet.ServletInputStream;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

final class BBjShipServletDevelopmentTest extends AdministratorRequest {

  private static final String APP_NAME = "hello-app";
  private static final String DEPLOYMENTS_FOLDER = "deployments/hello-app";
  private static final String CLASSPATH_NAME = "webforj_bbjship_hello-app";

  private Path app;
  private Path classFolder;
  private Path archive;
  private Path development;

  @BeforeEach
  void initDevelopmentArchiveOfHelloAppWithDevelopmentRunsEnabled() throws Exception {
    app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    classFolder = app.resolve("src/main/resources");
    archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    development = Files.copy(archive, temporary.resolve("development.zip"));
    try (FileSystem zip = FileSystems.newFileSystem(development)) {
      Files.delete(zip.getPath("application.jar"));
      Files.writeString(zip.getPath("development.properties"), """
          classFolder=%s
          """.formatted(classFolder.toString().replace("\\", "\\\\")));
    }

    Files.createFile(runtime.resolve("development.enabled"));
    when(request.getPathInfo()).thenReturn("/applications/hello-app");
  }

  @Test
  void shouldCreateTheDevelopmentRunAndAnswerItsLocation() throws Exception {
    ServletInputStream upload = mock(ServletInputStream.class,
        delegatesTo(new ByteArrayInputStream(Files.readAllBytes(development))));
    when(request.getMethod()).thenReturn("PUT");
    when(request.getInputStream()).thenReturn(upload);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(201);
    verify(response).setHeader("Location", "/webapp/hello-app");
    verify(admin).commit();
    Path deployments = runtime.resolve(DEPLOYMENTS_FOLDER);
    assertEquals(List.of(classFolder.toString()),
        DeploymentArchive
            .findClassFolders(deployments.resolve(Files.readString(deployments.resolve("current"))))
            .orElseThrow());
    assertEquals("""
        {
          "name": "hello-app",
          "url": "/webapp/hello-app"
        }""", JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @Test
  void shouldAnswerForbiddenWhileDevelopmentRunsAreDisabled() throws Exception {
    Files.delete(runtime.resolve("development.enabled"));
    ServletInputStream upload = mock(ServletInputStream.class,
        delegatesTo(new ByteArrayInputStream(Files.readAllBytes(development))));
    when(request.getMethod()).thenReturn("PUT");
    when(request.getInputStream()).thenReturn(upload);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(403);
    verify(admin, never()).commit();
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Forbidden",
          "status": 403,
          "detail": "Development runs are not enabled on this BBj machine. Run bbjship development \
        enable"
        }""", JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @Test
  void shouldAnswerBadRequestForRelativeClassFolder() throws Exception {
    try (FileSystem zip = FileSystems.newFileSystem(development)) {
      Files.writeString(zip.getPath("development.properties"), """
          classFolder=target/classes
          """);
    }

    ServletInputStream upload = mock(ServletInputStream.class,
        delegatesTo(new ByteArrayInputStream(Files.readAllBytes(development))));
    when(request.getMethod()).thenReturn("PUT");
    when(request.getInputStream()).thenReturn(upload);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(400);
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Bad Request",
          "status": 400,
          "detail": "BBj development class folder must be an absolute directory on the BBj \
        machine: target/classes"
        }""", JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @Test
  void shouldAnswerBadRequestForDevelopmentArchiveWithApplicationJar() throws Exception {
    try (FileSystem zip = FileSystems.newFileSystem(development)) {
      Files.copy(app.resolve("target/hello-app-1.0-SNAPSHOT.jar"), zip.getPath("application.jar"));
    }

    ServletInputStream upload = mock(ServletInputStream.class,
        delegatesTo(new ByteArrayInputStream(Files.readAllBytes(development))));
    when(request.getMethod()).thenReturn("PUT");
    when(request.getInputStream()).thenReturn(upload);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(400);
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Bad Request",
          "status": 400,
          "detail": "BBj development archive must not contain application.jar"
        }""", JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @ParameterizedTest
  @MethodSource("getModeConflicts")
  void shouldAnswerConflictForApplicationOfTheOtherMode(boolean developmentPublished, String detail)
      throws Exception {
    Path previous = Files.createDirectories(runtime.resolve(DEPLOYMENTS_FOLDER))
        .resolve(UUID.randomUUID().toString());
    DeploymentArchive.read(archive, previous);
    if (developmentPublished) {
      DevelopmentRunFiles.write(previous, classFolder);
    }

    BBjAdminAppDeploymentApplication registered = mock(BBjAdminAppDeploymentApplication.class);
    when(registered.getString(BBjAdminAppDeploymentApplication.NAME)).thenReturn(APP_NAME);
    when(registered.getString(BBjAdminAppDeploymentApplication.PROGRAM))
        .thenReturn(previous.resolve("bbj/webforj.bbj").toString());
    when(registered.getString(BBjAdminAppDeploymentApplication.CLASSPATH))
        .thenReturn(CLASSPATH_NAME);
    when(configuration.getApplications())
        .thenReturn(new BBjAdminAdapterList<>(List.of(registered)));
    ServletInputStream upload = mock(ServletInputStream.class, delegatesTo(new ByteArrayInputStream(
        Files.readAllBytes(developmentPublished ? archive : development))));
    when(request.getMethod()).thenReturn("PUT");
    when(request.getInputStream()).thenReturn(upload);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(409);
    verify(admin, never()).commit();
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Conflict",
          "status": 409,
          "detail": "%s"
        }""".formatted(detail),
        JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @Test
  void shouldRemoveTheDevelopmentRun() throws Exception {
    Path previous = Files.createDirectories(runtime.resolve(DEPLOYMENTS_FOLDER))
        .resolve(UUID.randomUUID().toString());
    DeploymentArchive.read(development, previous);
    DevelopmentRunFiles.write(previous, classFolder);
    BBjAdminAppDeploymentApplication registered = mock(BBjAdminAppDeploymentApplication.class);
    when(registered.getString(BBjAdminAppDeploymentApplication.NAME)).thenReturn(APP_NAME);
    when(registered.getString(BBjAdminAppDeploymentApplication.PROGRAM))
        .thenReturn(previous.resolve("bbj/webforj.bbj").toString());
    when(registered.getString(BBjAdminAppDeploymentApplication.CLASSPATH))
        .thenReturn(CLASSPATH_NAME);
    when(registered.getProperties()).thenReturn(Map.of());
    when(registered.getArguments()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(registered.getJavaArguments()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(configuration.getApplications())
        .thenReturn(new BBjAdminAdapterList<>(List.of(registered)));
    when(admin.getClasspath(CLASSPATH_NAME))
        .thenReturn(new BBjAdminAdapterList<>(List.of(classFolder.toString())));
    when(request.getMethod()).thenReturn("DELETE");

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(204);
    verify(configuration).removeApplication(APP_NAME);
    verify(admin).setClasspath(CLASSPATH_NAME, null);
    assertFalse(Files.exists(runtime.resolve(DEPLOYMENTS_FOLDER)));
  }

  private static Stream<Arguments> getModeConflicts() {
    return Stream.of(Arguments.of(true, "hello-app is a development run. Stop the watch first"),
        Arguments.of(false, "hello-app is deployed from an archive. Undeploy it first"));
  }
}
