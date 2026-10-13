package com.webforj.bbjship.assets;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.mockito.AdditionalAnswers.delegatesTo;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.bbjship.assets.model.AssetRequest;
import com.webforj.plugin.foundation.bbj.DeploymentArchive;
import com.webforj.plugin.foundation.bbj.HelloApp;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.UUID;
import javax.imageio.ImageIO;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;

final class AssetsServletTest {

  private static final String APP_NAME = "hello-app";
  private static final String APP_CSS_PATH = "/hello-app/static/app.css"; // NOSONAR

  @TempDir
  Path temporary;

  private Path root;
  private Path deployed;
  private HttpServletRequest request;
  private HttpServletResponse response;
  private ByteArrayOutputStream body;

  @BeforeEach
  void initHelloAppDeployed() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    root = temporary.resolve("plugins/.webforj-bbjship-deployments/deployments");
    String id = UUID.randomUUID().toString();
    deployed = Files.createDirectories(root.resolve(APP_NAME)).resolve(id);
    DeploymentArchive.read(archive, deployed);
    Files.writeString(root.resolve("hello-app/current"), id);
    request = mock(HttpServletRequest.class);
    response = mock(HttpServletResponse.class);
    body = new ByteArrayOutputStream();
    ServletOutputStream output = mock(ServletOutputStream.class, delegatesTo(body));
    when(response.getOutputStream()).thenReturn(output);
  }

  @AfterEach
  void clearBBjHome() {
    System.clearProperty("basis.BBjHome");
  }

  @Test
  void shouldServeTheAssetFromTheDeploymentsOfTheBbjHome() throws Exception {
    System.setProperty("basis.BBjHome", temporary.toString());
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn(APP_CSS_PATH);

    AssetsServlet.handle(request, response);

    verify(response).setStatus(200);
    assertArrayEquals(Files.readAllBytes(HelloApp.getFile("static/app.css")), body.toByteArray());
  }

  @Test
  void shouldServeTheStylesheetOfTheApplication() throws Exception {
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn(APP_CSS_PATH);

    AssetsServlet.handle(request, response, root);

    byte[] stylesheet = Files.readAllBytes(HelloApp.getFile("static/app.css"));
    verify(response).setStatus(200);
    verify(response).setContentType("text/css");
    verify(response).setContentLength(stylesheet.length);
    verify(response, never()).setHeader(anyString(), anyString());
    assertArrayEquals(stylesheet, body.toByteArray());
  }

  @Test
  void shouldAnswerHeadWithTheHeaderFieldsAndWithoutBody() throws Exception {
    when(request.getMethod()).thenReturn("HEAD");
    when(request.getPathInfo()).thenReturn("/hello-app/icons/icon.png");

    AssetsServlet.handle(request, response, root);

    verify(response).setStatus(200);
    verify(response).setContentType("image/png");
    verify(response).setHeader(eq("ETag"), anyString());
    verify(response)
        .setContentLength(Files.readAllBytes(HelloApp.getFile("icons/icon.png")).length);
    assertEquals(0, body.size());
  }

  @Test
  void shouldAnswerNotModifiedForTheEntityTagTheClientHolds() throws Exception {
    String entityTag = DeployedAssets
        .read(root.toString(), AssetRequest.create(APP_NAME, "icons/icon.png").build())
        .getEntityTag();
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn("/hello-app/icons/icon.png");
    when(request.getHeader("If-None-Match")).thenReturn(entityTag);

    AssetsServlet.handle(request, response, root);

    verify(response).setStatus(304);
    verify(response).setHeader("ETag", entityTag);
    verify(response, never()).setContentType(anyString());
    verify(response, never()).getOutputStream();
  }

  @Test
  void shouldScaleTheIconWithThePaddingAndBackgroundOfTheQuery() throws Exception {
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn("/hello-app/icons/icon-20x20.png");
    when(request.getParameter("padding")).thenReturn(".5");
    when(request.getParameter("background")).thenReturn("00ff00");

    AssetsServlet.handle(request, response, root);

    BufferedImage icon = ImageIO.read(new ByteArrayInputStream(body.toByteArray()));
    verify(response).setStatus(200);
    assertEquals(20, icon.getWidth());
    assertEquals(Color.GREEN.getRGB(), icon.getRGB(1, 1));
    assertNotEquals(Color.GREEN.getRGB(), icon.getRGB(10, 10));
  }

  @ParameterizedTest
  @ValueSource(strings = {"POST", "PUT", "DELETE"})
  void shouldAnswerMethodNotAllowedForMethodOtherThanGetAndHead(String method) throws Exception {
    when(request.getMethod()).thenReturn(method);
    when(request.getPathInfo()).thenReturn(APP_CSS_PATH);

    AssetsServlet.handle(request, response, root);

    verify(response).sendError(405, "Only GET and HEAD requests are supported");
    verify(response, never()).getOutputStream();
  }

  @ParameterizedTest
  @NullSource
  @ValueSource(strings = {"", "/", "/hello-app", "/hello-app/", "//static/app.css",
      "/missing-app/static/app.css", "/hello-app/static/missing.css"})
  void shouldAnswerNotFoundForPathThatNamesNoAsset(String path) throws Exception {
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn(path);

    AssetsServlet.handle(request, response, root);

    verify(response).sendError(404, "The asset does not exist");
    verify(response, never()).setStatus(anyInt());
    verify(response, never()).getOutputStream();
  }

  @Test
  void shouldAnswerInternalServerErrorWhenTheDeploymentCannotBeRead() throws Exception {
    Files.writeString(deployed.resolve("application.jar"), APP_NAME);
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn(APP_CSS_PATH);

    AssetsServlet.handle(request, response, root);

    verify(response).sendError(500, "The asset could not be read. Check the BBjServices log");
    verify(response, never()).getOutputStream();
  }
}
