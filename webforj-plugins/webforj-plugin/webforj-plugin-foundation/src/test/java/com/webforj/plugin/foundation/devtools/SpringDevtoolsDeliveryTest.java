package com.webforj.plugin.foundation.devtools;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.plugin.foundation.resolve.ApplicationClasspath;
import com.webforj.plugin.foundation.resolve.ApplicationClasspath.ResolvedJar;
import com.webforj.plugin.foundation.resolve.ArtifactResolver;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;

class SpringDevtoolsDeliveryTest {

  private static final String VERSION = "26.02-SNAPSHOT";

  @Test
  void shouldSkipTheApplicationWithoutTheFramework() throws Exception {
    List<String> diagnostics = new ArrayList<>();
    SpringDevtoolsDelivery delivery = SpringDevtoolsDelivery.create()
        .setApplicationClasspath(new ApplicationClasspath(
            List.of(jar("org.springframework", "spring-core", "6.2.0", "spring.jar"))))
        .setResolver((groupId, artifactId, version) -> {
          throw new AssertionError("nothing resolves for the application without the framework");
        }).setDebug(diagnostics::add).build();

    assertTrue(delivery.getJars().isEmpty());
    assertEquals(1, diagnostics.size(), "the skip is reported to the diagnostic sink");
  }

  @Test
  void shouldKeepOnlyTheDevtoolsDelta() throws Exception {
    ApplicationClasspath application = new ApplicationClasspath(
        List.of(jar("com.webforj", "webforj-foundation", VERSION, "webforj-foundation.jar"),
            jar("org.java-websocket", "Java-WebSocket", "1.6.0", "Java-WebSocket.jar")));

    ArtifactResolver resolver = (groupId, artifactId, version) -> {
      if (ApplicationClasspath.FRAMEWORK_ARTIFACT_ID.equals(artifactId)) {
        return List
            .of(jar("com.fasterxml.jackson.core", "jackson-core", "2.21.4", "jackson-core.jar"));
      }

      return List.of(
          jar("com.webforj", "webforj-spring-devtools", VERSION, "webforj-spring-devtools.jar"),
          jar("org.java-websocket", "Java-WebSocket", "1.6.0", "dupe.jar"),
          jar("com.fasterxml.jackson.core", "jackson-core", "2.21.4", "jackson-dupe.jar"));
    };

    List<String> lines = new ArrayList<>();
    List<Path> additions = SpringDevtoolsDelivery.create().setApplicationClasspath(application)
        .setResolver(resolver).setLog(lines::add).build().getJars();

    assertEquals(List.of(Path.of("webforj-spring-devtools.jar")), additions,
        "a jar the application classpath or the framework tree already carries is never added");
    assertEquals(1, lines.size(), "the delivery reports what it hands over");
    assertTrue(lines.get(0).contains("1"));
  }

  @Test
  void shouldStayQuietWhenTheApplicationCarriesEverything() throws Exception {
    ApplicationClasspath application = new ApplicationClasspath(
        List.of(jar("com.webforj", "webforj-foundation", VERSION, "webforj-foundation.jar"),
            jar("com.webforj", "webforj-spring-devtools", VERSION, "carried.jar")));

    ArtifactResolver resolver = (groupId, artifactId, version) -> List
        .of(jar("com.webforj", "webforj-spring-devtools", VERSION, "webforj-spring-devtools.jar"));

    List<String> lines = new ArrayList<>();
    List<Path> additions = SpringDevtoolsDelivery.create().setApplicationClasspath(application)
        .setResolver(resolver).setLog(lines::add).build().getJars();

    assertTrue(additions.isEmpty());
    assertTrue(lines.isEmpty(), "nothing is reported when nothing is handed over");
  }

  private static ResolvedJar jar(String groupId, String artifactId, String version,
      String fileName) {
    return new ResolvedJar(groupId, artifactId, version, Path.of(fileName));
  }
}
