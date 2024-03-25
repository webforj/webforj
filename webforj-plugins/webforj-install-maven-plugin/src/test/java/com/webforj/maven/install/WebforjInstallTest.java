package com.webforj.maven.install;


import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.containing;
import static com.github.tomakehurst.wiremock.client.WireMock.notFound;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.stubFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import com.github.tomakehurst.wiremock.junit5.WireMockTest;
import java.io.File;
import java.lang.reflect.Field;
import java.util.Objects;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.platform.commons.util.ReflectionUtils;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@WireMockTest(httpPort = 9191, proxyMode = true)
@ExtendWith(MockitoExtension.class)
class WebforjInstallTest {

  @Mock
  private MavenProject mavenProject;

  @Mock
  private Artifact artifact;

  WebforjInstall webforjInstall;
  private String defaultDeployurl;

  @BeforeEach
  public void setup() {
    webforjInstall = new WebforjInstall();
    defaultDeployurl = "http://localhost:9191/webforj-install";
  }

  @AfterEach
  public void tearDown() {
  }

  @Test
  void test_null_deployurl_throws_IllegalArgumentException() {
    IllegalArgumentException exception =
        assertThrows(IllegalArgumentException.class, () -> webforjInstall.execute());
    assertEquals("deployurl is null!", exception.getMessage());
  }

  @Test
  void test_null_project_throws_IllegalArgumentException() throws IllegalAccessException {
    setField(webforjInstall, "deployurl", defaultDeployurl);
    IllegalArgumentException exception =
        assertThrows(IllegalArgumentException.class, () -> webforjInstall.execute());
    assertEquals("project is null!", exception.getMessage());
  }

  @Test
  void test_null_application_throws_IllegalArgumentException() throws IllegalAccessException {
    setField(webforjInstall, "deployurl", defaultDeployurl);
    setField(webforjInstall, "project", mavenProject);
    when(mavenProject.getArtifact()).thenReturn(null);
    IllegalArgumentException exception =
        assertThrows(IllegalArgumentException.class, () -> webforjInstall.execute());
    assertEquals("project artifact is null!", exception.getMessage());
  }

  @Test
  void test_null_file_throws_IllegalArgumentException() throws IllegalAccessException {
    setField(webforjInstall, "deployurl", defaultDeployurl);
    setField(webforjInstall, "project", mavenProject);
    when(mavenProject.getArtifact()).thenReturn(artifact);
    when(artifact.getFile()).thenReturn(null);
    NullPointerException exception =
        assertThrows(NullPointerException.class, () -> webforjInstall.execute());
    assertEquals("artifact file is null!", exception.getMessage());
  }

  @Test
  void test_post_valid_jar() throws Exception {
    File testFile = getResourceFile();
    assertTrue(testFile.exists());
    stubFor(post(urlEqualTo("/webforj-install"))
        .withHeader("Content-Type", containing("multipart/form-data")) //
        .withRequestBody(containing("jar")) //
        .willReturn(aResponse().withStatus(200)));
    setField(webforjInstall, "deployurl", defaultDeployurl);
    setField(webforjInstall, "project", mavenProject);
    when(mavenProject.getArtifact()).thenReturn(artifact);
    when(artifact.getFile()).thenReturn(testFile);
    webforjInstall.execute();
  }

  @Test
  void test_post_invalid_http_status() throws Exception {
    File testFile = getResourceFile();
    assertTrue(testFile.exists());
    stubFor(post(urlEqualTo("/webforj-install"))
        .withHeader("Content-Type", containing("multipart/form-data")) //
        .withRequestBody(containing("jar")) //
        .willReturn(notFound()));
    setField(webforjInstall, "deployurl", defaultDeployurl);
    setField(webforjInstall, "project", mavenProject);
    when(mavenProject.getArtifact()).thenReturn(artifact);
    when(artifact.getFile()).thenReturn(testFile);
    webforjInstall.execute();
  }

  @Test
  void test_post_invalid_url() throws IllegalAccessException {
    File testFile = getResourceFile();
    assertTrue(testFile.exists());
    stubFor(post(urlEqualTo("/webforj-install"))
        .withHeader("Content-Type", containing("multipart/form-data")) //
        .withRequestBody(containing("jar")) //
        .willReturn(aResponse().withStatus(200)));
    setField(webforjInstall, "deployurl", "http://doesnotexit");
    setField(webforjInstall, "project", mavenProject);
    when(mavenProject.getArtifact()).thenReturn(artifact);
    when(artifact.getFile()).thenReturn(testFile);
    MojoExecutionException mojoException =
        assertThrows(MojoExecutionException.class, () -> webforjInstall.execute());
    assertTrue(mojoException.getMessage().contains(
      "java.net.UnknownHostException: doesnotexit: nodename nor servname provided, or not known"));
  }

  /**
   * Commonly shared among the tests.
   *
   * @return the file.
   */
  File getResourceFile() {
    return new File(
      Objects.requireNonNull(getClass().getClassLoader().getResource("test.jar")).getFile());
  }

  /**
   * Set a field value.
   *
   * @param webforjInstall the instance of the class being tested.
   * @param fieldName the name of the field to set.
   * @param fieldValue the value of the field to set.
   * @throws IllegalAccessException setting field.
   */
  void setField(WebforjInstall webforjInstall, String fieldName, Object fieldValue)
    throws IllegalAccessException {
      Field field =
        ReflectionUtils.streamFields(WebforjInstall.class, f -> fieldName.equals(f.getName()),
          ReflectionUtils.HierarchyTraversalMode.TOP_DOWN).toList().get(0);
      field.setAccessible(true);
      field.set(webforjInstall, fieldValue);
  }
}
