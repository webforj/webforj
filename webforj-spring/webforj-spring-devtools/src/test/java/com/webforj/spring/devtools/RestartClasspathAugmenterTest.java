package com.webforj.spring.devtools;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.FileOutputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class RestartClasspathAugmenterTest {

  private final RestartClasspathAugmenter augmenter = new RestartClasspathAugmenter();

  @Test
  void shouldDetectJarThatDeclaresWebforjDependency(@TempDir Path dir) throws Exception {
    File jar = jarWithPom(dir, "acme-addon-1.0.jar", "com.acme", "acme-addon",
        "<dependency><groupId>com.webforj</groupId></dependency>");

    List<URL> result = augmenter.findWebforjDependentJars(List.of(jar));

    assertEquals(List.of(jar.toURI().toURL()), result);
  }

  @Test
  void shouldIgnoreJarWithoutWebforjDependency(@TempDir Path dir) throws Exception {
    File jar = jarWithPom(dir, "other-1.0.jar", "com.other", "other",
        "<dependency><groupId>org.apache.commons</groupId></dependency>");

    assertTrue(augmenter.findWebforjDependentJars(List.of(jar)).isEmpty());
  }

  @Test
  void shouldIgnoreJarWithoutMavenDescriptor(@TempDir Path dir) throws Exception {
    File jar = jarWithEntry(dir, "nodescriptor-1.0.jar", "com/acme/Foo.class", "data");

    assertTrue(augmenter.findWebforjDependentJars(List.of(jar)).isEmpty());
  }

  @Test
  void shouldIgnoreWebforjOwnJarsMatchedByName(@TempDir Path dir) throws Exception {
    File jar = jarWithPom(dir, "webforj-foundation-26.01.jar", "com.webforj", "webforj-foundation",
        "<dependency><groupId>com.webforj</groupId></dependency>");

    assertTrue(augmenter.findWebforjDependentJars(List.of(jar)).isEmpty());
  }

  @Test
  void shouldReturnOnlyDependentJarsFromMixedClasspath(@TempDir Path dir) throws Exception {
    File dependent = jarWithPom(dir, "addon-1.0.jar", "com.acme", "addon",
        "<dependency><groupId>com.webforj</groupId></dependency>");
    File unrelated = jarWithPom(dir, "lib-1.0.jar", "com.lib", "lib",
        "<dependency><groupId>org.slf4j</groupId></dependency>");

    assertEquals(List.of(dependent.toURI().toURL()),
        augmenter.findWebforjDependentJars(List.of(dependent, unrelated)));
  }

  @Test
  void shouldNotMatchWebforjMentionedOnlyInText(@TempDir Path dir) throws Exception {
    File jar = jarWithPom(dir, "other-1.0.jar", "com.other", "other",
        "<!-- com.webforj --><dependency><groupId>org.other</groupId></dependency>");

    assertTrue(augmenter.findWebforjDependentJars(List.of(jar)).isEmpty());
  }

  @Test
  void shouldNotMatchSimilarGroupId(@TempDir Path dir) throws Exception {
    File jar = jarWithPom(dir, "other-1.0.jar", "com.other", "other",
        "<dependency><groupId>com.webforjx</groupId></dependency>");

    assertTrue(augmenter.findWebforjDependentJars(List.of(jar)).isEmpty());
  }

  private static File jarWithPom(Path dir, String jarName, String groupId, String artifactId,
      String dependenciesXml) throws Exception {
    String pom = "<project><modelVersion>4.0.0</modelVersion><groupId>" + groupId
        + "</groupId><artifactId>" + artifactId + "</artifactId><dependencies>" + dependenciesXml
        + "</dependencies></project>";
    String entry = "META-INF/maven/" + groupId + "/" + artifactId + "/pom.xml";

    return jarWithEntry(dir, jarName, entry, pom);
  }

  private static File jarWithEntry(Path dir, String jarName, String entryName, String content)
      throws Exception {
    File jar = dir.resolve(jarName).toFile();

    try (ZipOutputStream zip = new ZipOutputStream(new FileOutputStream(jar))) {
      zip.putNextEntry(new ZipEntry(entryName));
      zip.write(content.getBytes(StandardCharsets.UTF_8));
      zip.closeEntry();
    }

    return jar;
  }
}
