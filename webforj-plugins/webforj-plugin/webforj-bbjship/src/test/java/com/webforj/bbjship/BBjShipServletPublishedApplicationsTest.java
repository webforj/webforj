package com.webforj.bbjship;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.AdditionalAnswers.delegatesTo;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminAppDeploymentApplication;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.google.gson.JsonParser;
import com.webforj.plugin.foundation.bbj.DeploymentArchive;
import com.webforj.plugin.foundation.bbj.HelloApp;
import jakarta.servlet.ServletInputStream;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

final class BBjShipServletPublishedApplicationsTest extends AdministratorRequest {

  private Path archive;

  @BeforeEach
  void initServerWithHelloAppPublished() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    Path previous = Files.createDirectories(runtime.resolve("deployments/hello-app"))
        .resolve(UUID.randomUUID().toString());
    DeploymentArchive.read(archive, previous);
    Files.writeString(previous.resolveSibling("current"), previous.getFileName().toString());
    BBjAdminAppDeploymentApplication registered = mock(BBjAdminAppDeploymentApplication.class);
    when(registered.getString(BBjAdminAppDeploymentApplication.NAME)).thenReturn("hello-app");
    when(registered.getString(BBjAdminAppDeploymentApplication.PROGRAM))
        .thenReturn(previous.resolve("bbj/webforj.bbj").toString());
    when(registered.getString(BBjAdminAppDeploymentApplication.CLASSPATH))
        .thenReturn("webforj_bbjship_hello-app");
    when(registered.getArguments()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(registered.getJavaArguments()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(configuration.getApplications())
        .thenReturn(new BBjAdminAdapterList<>(List.of(registered)));
    when(admin.getClasspathNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("webforj_bbjship_hello-app")));
    when(admin.getClasspath("webforj_bbjship_hello-app"))
        .thenReturn(new BBjAdminAdapterList<>(List.of()));
  }

  @Test
  void shouldReplaceTheApplicationAndAnswerOk() throws Exception {
    ServletInputStream upload = mock(ServletInputStream.class,
        delegatesTo(new ByteArrayInputStream(Files.readAllBytes(archive))));
    when(request.getMethod()).thenReturn("PUT");
    when(request.getPathInfo()).thenReturn("/applications/hello-app");
    when(request.getInputStream()).thenReturn(upload);

    BBjShipServlet.handle(request, response, plugin, sessions);

    verify(response).setStatus(200);
    verify(response).setHeader("Location", "/webapp/hello-app");
    verify(configuration).removeApplication("hello-app");
    assertEquals("""
        {
          "name": "hello-app",
          "url": "/webapp/hello-app"
        }""", JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @Test
  void shouldAnswerConflictForEntryClassPublishedUnderAnotherName() throws Exception {
    ServletInputStream upload = mock(ServletInputStream.class,
        delegatesTo(new ByteArrayInputStream(Files.readAllBytes(archive))));
    when(request.getMethod()).thenReturn("PUT");
    when(request.getPathInfo()).thenReturn("/applications/second-app");
    when(request.getInputStream()).thenReturn(upload);

    BBjShipServlet.handle(request, response, plugin, sessions);

    verify(response).setStatus(409);
    verify(configuration, never()).createApplication();
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Conflict",
          "status": 409,
          "detail": "Entry class org.example.Application is already published as hello-app. \
        Undeploy hello-app first"
        }""", JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @Test
  void shouldRemoveTheApplicationAndAnswerWithoutContent() throws Exception {
    when(request.getMethod()).thenReturn("DELETE");
    when(request.getPathInfo()).thenReturn("/applications/hello-app");

    BBjShipServlet.handle(request, response, plugin, sessions);

    verify(response).setStatus(204);
    verify(response, never()).setContentType(anyString());
    verify(response, never()).setContentLength(anyInt());
    verify(configuration).removeApplication("hello-app");
    verify(admin).release();
    assertEquals(0, body.size());
    try (Stream<Path> folders = Files.list(runtime.resolve("deployments"))) {
      assertEquals(List.of(runtime.resolve("deployments/.lock")), folders.toList());
    }
  }
}
