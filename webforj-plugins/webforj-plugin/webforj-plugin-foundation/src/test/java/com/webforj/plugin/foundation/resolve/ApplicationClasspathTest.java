package com.webforj.plugin.foundation.resolve;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.plugin.foundation.resolve.ApplicationClasspath.ResolvedJar;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.junit.jupiter.api.Test;

class ApplicationClasspathTest {

  private static final String VERSION = "26.02-SNAPSHOT";

  @Test
  void shouldFindTheFrameworkVersionOnlyWhenTheFrameworkIsPresent() {
    ResolvedJar spring = jar("org.springframework", "spring-core", "6.2.0");
    ResolvedJar framework = jar("com.webforj", "webforj-foundation", VERSION);

    assertEquals(Optional.of(VERSION),
        new ApplicationClasspath(List.of(spring, framework)).getFrameworkVersion());
    assertTrue(new ApplicationClasspath(List.of(spring)).getFrameworkVersion().isEmpty());
  }

  @Test
  void shouldListTheModuleKeysInClasspathOrderWithoutRepetition() {
    ApplicationClasspath classpath =
        new ApplicationClasspath(List.of(jar("com.webforj", "webforj-foundation", VERSION),
            jar("org.springframework", "spring-core", "6.2.0"),
            jar("org.springframework", "spring-core", "6.2.0")));

    assertEquals(Set.of("com.webforj:webforj-foundation", "org.springframework:spring-core"),
        classpath.getModuleKeys());
    assertEquals(List.of("com.webforj:webforj-foundation", "org.springframework:spring-core"),
        List.copyOf(classpath.getModuleKeys()));
  }

  private static ResolvedJar jar(String groupId, String artifactId, String version) {
    return new ResolvedJar(groupId, artifactId, version, Path.of(artifactId + ".jar"));
  }
}
