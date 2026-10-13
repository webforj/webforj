package com.webforj.bbjship;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.AdditionalAnswers.delegatesTo;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.google.gson.JsonParser;
import com.webforj.bbjship.DeploymentException.Reason;
import com.webforj.plugin.foundation.bbj.DeploymentArchive;
import com.webforj.plugin.foundation.bbj.DeploymentArchive.ArchiveSizeException;
import com.webforj.plugin.foundation.bbj.HelloApp;
import jakarta.servlet.ServletInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

final class BBjShipServletDeploymentTest extends AdministratorRequest {

  private static final String APP_NAME = "hello-app";
  private static final String ARCHIVE_NAME = "hello-app.zip";
  private static final String APPLICATION_PATH = "/applications/hello-app";

  @Test
  void shouldCreateTheApplicationAndAnswerItsLocation() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
    ServletInputStream upload = mock(ServletInputStream.class,
        delegatesTo(new ByteArrayInputStream(Files.readAllBytes(archive))));
    when(request.getMethod()).thenReturn("PUT");
    when(request.getPathInfo()).thenReturn(APPLICATION_PATH);
    when(request.getInputStream()).thenReturn(upload);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(201);
    verify(response).setHeader("Location", "/webapp/hello-app");
    verify(response).setContentType("application/json");
    verify(admin).commit();
    verify(admin).release();
    assertTrue(Files.isRegularFile(runtime.resolve("deployments/hello-app/current")));
    assertEquals("""
        {
          "name": "hello-app",
          "url": "/webapp/hello-app"
        }""", JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @Test
  void shouldAnswerBadRequestForBodyThatIsNotAnArchive() throws Exception {
    ServletInputStream upload = mock(ServletInputStream.class,
        delegatesTo(new ByteArrayInputStream(APP_NAME.getBytes(StandardCharsets.UTF_8))));
    when(request.getMethod()).thenReturn("PUT");
    when(request.getPathInfo()).thenReturn(APPLICATION_PATH);
    when(request.getInputStream()).thenReturn(upload);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(400);
    verify(response).setContentType("application/problem+json");
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Bad Request",
          "status": 400,
          "detail": "The uploaded body is not a BBj deployment archive. Deploy the ZIP archive \
        the build writes"
        }""", JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @Test
  void shouldAnswerBadRequestForInvalidWebforjConf() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Files.writeString(app.resolve("src/main/resources/webforj.conf"), """
        webforj.entry = "org.example.Application"
        webforj.quiet = [true
        """);
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
    ServletInputStream upload = mock(ServletInputStream.class,
        delegatesTo(new ByteArrayInputStream(Files.readAllBytes(archive))));
    when(request.getMethod()).thenReturn("PUT");
    when(request.getPathInfo()).thenReturn(APPLICATION_PATH);
    when(request.getInputStream()).thenReturn(upload);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(400);
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Bad Request",
          "status": 400,
          "detail": "BBj deployment webforj.conf is invalid. Reader: 3: List should have had \
        new element after a comma, instead had token: end of file (if you want the comma or end \
        of file to be part of a string value, then double-quote it)"
        }""", JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @Test
  void shouldAnswerCreatedForRelativeConfigFile() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Files.writeString(app.resolve("src/main/resources/webforj.conf"), """
        webforj.entry = "org.example.Application"
        webforj.configFile = "config/app.cfg"
        """);
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
    ServletInputStream upload = mock(ServletInputStream.class,
        delegatesTo(new ByteArrayInputStream(Files.readAllBytes(archive))));
    when(request.getMethod()).thenReturn("PUT");
    when(request.getPathInfo()).thenReturn(APPLICATION_PATH);
    when(request.getInputStream()).thenReturn(upload);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(201);
    verify(admin).commit();
  }

  @Test
  void shouldAnswerConflictForNameThatDiffersOnlyByLetterCase() throws Exception {
    Files.createDirectories(runtime.resolve("deployments/Hello-App"));
    when(request.getMethod()).thenReturn("PUT");
    when(request.getPathInfo()).thenReturn(APPLICATION_PATH);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(409);
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Conflict",
          "status": 409,
          "detail": "Published BBj application Hello-App differs from hello-app only by letter \
        case. Choose another publish name"
        }""", JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @Test
  void shouldAnswerContentTooLargeWhenTheUploadExceedsTheLimit() throws Exception {
    ServletInputStream upload = mock(ServletInputStream.class);
    when(upload.read(any(byte[].class))).thenThrow(new DeploymentException(Reason.ARCHIVE_TOO_LARGE,
        "BBj deployment archive exceeds the allowed size of 1 GiB"));
    when(request.getMethod()).thenReturn("PUT");
    when(request.getPathInfo()).thenReturn(APPLICATION_PATH);
    when(request.getInputStream()).thenReturn(upload);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(413);
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Content Too Large",
          "status": 413,
          "detail": "BBj deployment archive exceeds the allowed size of 1 GiB"
        }""", JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @Test
  void shouldAnswerContentTooLargeWhenTheArchiveUnpacksBeyondTheLimit() throws Exception {
    ArchiveSizeException tooLarge = mock(ArchiveSizeException.class);
    when(tooLarge.getMessage()).thenReturn(DeploymentArchive.TOO_LARGE_MESSAGE);
    ServletInputStream upload = mock(ServletInputStream.class);
    when(upload.read(any(byte[].class))).thenThrow(tooLarge);
    when(request.getMethod()).thenReturn("PUT");
    when(request.getPathInfo()).thenReturn(APPLICATION_PATH);
    when(request.getInputStream()).thenReturn(upload);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(413);
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Content Too Large",
          "status": 413,
          "detail": "BBj deployment archive exceeds the allowed size of 1 GiB"
        }""", JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @ParameterizedTest
  @MethodSource("getUnexpectedFailures")
  void shouldAnswerInternalServerErrorWithoutTheRawError(Exception failure) throws Exception {
    when(request.getMethod()).thenReturn("PUT");
    when(request.getPathInfo()).thenReturn(APPLICATION_PATH);
    when(request.getInputStream()).thenThrow(failure);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(500);
    verify(admin).release();
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Internal Server Error",
          "status": 500,
          "detail": "BBj deployment failed. Check the BBjServices log"
        }""", JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @Test
  void shouldAnswerNotFoundForRemovalOfApplicationTheBbjShipDidNotDeploy() throws Exception {
    when(request.getMethod()).thenReturn("DELETE");
    when(request.getPathInfo()).thenReturn(APPLICATION_PATH);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(404);
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Not Found",
          "status": 404,
          "detail": "No BBj application named hello-app was deployed by BBjShip. Check the \
        publish name"
        }""", JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  private static Stream<Arguments> getUnexpectedFailures() {
    return Stream.of(Arguments.of(new IOException("disk full at /secret/path")),
        Arguments.of(new IllegalStateException("getReader was already called")));
  }
}
