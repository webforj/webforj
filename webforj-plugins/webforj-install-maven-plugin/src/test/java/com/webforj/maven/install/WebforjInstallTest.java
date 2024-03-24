package com.webforj.maven.install;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

import com.github.tomakehurst.wiremock.junit5.WireMockTest;
import java.lang.reflect.Field;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.platform.commons.util.ReflectionUtils;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@WireMockTest(httpsEnabled = true, httpPort = 9191, proxyMode = true)
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
    defaultDeployurl = "http://localhost:9191/dwcj-install";
  }

  @AfterEach
  public void tearDown()  {}

  @Test
  void test_null_deployurl_throws_IllegalArgumentException() {
    IllegalArgumentException exception =
        assertThrows(IllegalArgumentException.class, () -> webforjInstall.execute());
    assertEquals("deployurl is null!", exception.getMessage());
  }

  @Test
  void test_null_project_throws_IllegalArgumentException()
      throws Exception {
    setField(webforjInstall, "deployurl", defaultDeployurl);
    IllegalArgumentException exception =
        assertThrows(IllegalArgumentException.class, () -> webforjInstall.execute());
    assertEquals("project is null!", exception.getMessage());
  }

  @Test
  void test_null_application_throws_IllegalArgumentException()
      throws Exception {
    setField(webforjInstall, "deployurl", defaultDeployurl);
    setField(webforjInstall, "project", mavenProject);
    when(mavenProject.getArtifact()).thenReturn(null);
    IllegalArgumentException exception =
        assertThrows(IllegalArgumentException.class, () -> webforjInstall.execute());
    assertEquals("project artifact is null!", exception.getMessage());
  }

  @Test
  void test_null_file_throws_IllegalArgumentException()
      throws Exception {
    setField(webforjInstall, "deployurl", defaultDeployurl);
    setField(webforjInstall, "project", mavenProject);
    when(mavenProject.getArtifact()).thenReturn(artifact);
    when(artifact.getFile()).thenReturn(null);
    NullPointerException exception =
        assertThrows(NullPointerException.class, () -> webforjInstall.execute());
    assertEquals("artifact file is null!", exception.getMessage());
  }


  void setField(WebforjInstall webforjInstall, String fieldName, Object fieldValue)
      throws IllegalAccessException {
    System.out.println("fieldName = " + fieldName);
    Field field =
        ReflectionUtils.streamFields(WebforjInstall.class, f -> fieldName.equals(f.getName()),
            ReflectionUtils.HierarchyTraversalMode.TOP_DOWN).toList().get(0);
    field.setAccessible(true);
    field.set(webforjInstall, fieldValue);
  }
}
