package com.webforj.bbjship.foundation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bbjship.foundation.ApplicationClasspath.ResolvedJar;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.junit.jupiter.api.Test;

class ApplicationClasspathTest {

  private static final String VERSION = "26.02-SNAPSHOT";
  private static final String SPRING_ARTIFACT = "spring-core";
  private static final String SPRING_GROUP_ID = "org.springframework";
  private static final String SPRING_VERSION = "6.2.0";

  @Test
  void shouldFindTheFrameworkVersionOnlyWhenTheFrameworkIsPresent() {
    ResolvedJar spring = jar(SPRING_GROUP_ID, SPRING_ARTIFACT, SPRING_VERSION);
    ResolvedJar framework = jar("com.webforj", "webforj-foundation", VERSION);

    assertEquals(Optional.of(VERSION),
        new ApplicationClasspath(List.of(spring, framework)).getFrameworkVersion());
    assertTrue(new ApplicationClasspath(List.of(spring)).getFrameworkVersion().isEmpty());
  }

  @Test
  void shouldListTheModuleKeysInClasspathOrderWithoutRepetition() {
    ApplicationClasspath classpath =
        new ApplicationClasspath(List.of(jar("com.webforj", "webforj-foundation", VERSION),
            jar(SPRING_GROUP_ID, SPRING_ARTIFACT, SPRING_VERSION),
            jar(SPRING_GROUP_ID, SPRING_ARTIFACT, SPRING_VERSION)));

    assertEquals(Set.of("com.webforj:webforj-foundation", "org.springframework:spring-core"),
        classpath.getModuleKeys());
    assertEquals(List.of("com.webforj:webforj-foundation", "org.springframework:spring-core"),
        List.copyOf(classpath.getModuleKeys()));
  }

  private static ResolvedJar jar(String groupId, String artifactId, String version) {
    return new ResolvedJar(groupId, artifactId, version, Path.of(artifactId + ".jar"));
  }
}
