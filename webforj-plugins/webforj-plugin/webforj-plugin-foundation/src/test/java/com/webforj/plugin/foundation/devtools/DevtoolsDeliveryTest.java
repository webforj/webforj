package com.webforj.plugin.foundation.devtools;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bbjship.foundation.ApplicationClasspath;
import com.webforj.bbjship.foundation.ApplicationClasspath.ResolvedJar;
import com.webforj.plugin.foundation.resolve.ArtifactResolver;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;

class DevtoolsDeliveryTest {

  private static final String VERSION = "26.02-SNAPSHOT";
  private static final String SPRING_DEVTOOLS_ARTIFACT = "webforj-spring-devtools";
  private static final String GROUP_ID = "com.webforj";
  private static final String FOUNDATION_JAR = "webforj-foundation.jar";
  private static final String FOUNDATION_ARTIFACT = "webforj-foundation";
  private static final String WEBSOCKET_VERSION = "1.6.0";
  private static final String WEBSOCKET_JAR = "Java-WebSocket.jar";
  private static final String WEBSOCKET_ARTIFACT = "Java-WebSocket";
  private static final String WEBSOCKET_GROUP_ID = "org.java-websocket";
  private static final String SPRING_DEVTOOLS_JAR = "webforj-spring-devtools.jar";
  private static final String CARRIED_JAR = "carried.jar";
  private static final String DEVTOOLS_ARTIFACT = "webforj-devtools";

  @Test
  void shouldSkipTheApplicationWithoutTheFramework() throws Exception {
    List<String> diagnostics = new ArrayList<>();
    DevtoolsDelivery delivery = DevtoolsDelivery.create().setArtifactId(SPRING_DEVTOOLS_ARTIFACT)
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
        List.of(jar(GROUP_ID, FOUNDATION_ARTIFACT, VERSION, FOUNDATION_JAR),
            jar(WEBSOCKET_GROUP_ID, WEBSOCKET_ARTIFACT, WEBSOCKET_VERSION, WEBSOCKET_JAR)));

    ArtifactResolver resolver = (groupId, artifactId, version) -> {
      if (ApplicationClasspath.FRAMEWORK_ARTIFACT_ID.equals(artifactId)) {
        return List
            .of(jar("com.fasterxml.jackson.core", "jackson-core", "2.21.4", "jackson-core.jar"));
      }

      return List.of(jar(GROUP_ID, SPRING_DEVTOOLS_ARTIFACT, VERSION, SPRING_DEVTOOLS_JAR),
          jar(WEBSOCKET_GROUP_ID, WEBSOCKET_ARTIFACT, WEBSOCKET_VERSION, "dupe.jar"),
          jar("com.fasterxml.jackson.core", "jackson-core", "2.21.4", "jackson-dupe.jar"));
    };

    List<String> lines = new ArrayList<>();
    List<ResolvedJar> additions = DevtoolsDelivery.create().setArtifactId(SPRING_DEVTOOLS_ARTIFACT)
        .setApplicationClasspath(application).setResolver(resolver).setLog(lines::add).build()
        .getJars();

    assertEquals(List.of(jar(GROUP_ID, SPRING_DEVTOOLS_ARTIFACT, VERSION, SPRING_DEVTOOLS_JAR)),
        additions,
        "a jar the application classpath or the framework tree already carries is never added");
    assertEquals(1, lines.size(), "the delivery reports what it hands over");
    assertTrue(lines.get(0).contains("1"));
  }

  @Test
  void shouldStayQuietWhenTheApplicationCarriesEverything() throws Exception {
    ApplicationClasspath application = new ApplicationClasspath(
        List.of(jar(GROUP_ID, FOUNDATION_ARTIFACT, VERSION, FOUNDATION_JAR),
            jar(GROUP_ID, SPRING_DEVTOOLS_ARTIFACT, VERSION, CARRIED_JAR)));

    ArtifactResolver resolver = (groupId, artifactId, version) -> List
        .of(jar(GROUP_ID, SPRING_DEVTOOLS_ARTIFACT, VERSION, SPRING_DEVTOOLS_JAR));

    List<String> lines = new ArrayList<>();
    List<ResolvedJar> additions = DevtoolsDelivery.create().setArtifactId(SPRING_DEVTOOLS_ARTIFACT)
        .setApplicationClasspath(application).setResolver(resolver).setLog(lines::add).build()
        .getJars();

    assertTrue(additions.isEmpty());
    assertTrue(lines.isEmpty(), "nothing is reported when nothing is handed over");
  }

  @Test
  void shouldRefuseMissingArtifactId() {
    DevtoolsDelivery.Builder builder =
        DevtoolsDelivery.create().setApplicationClasspath(new ApplicationClasspath(List.of()))
            .setResolver((groupId, artifactId, version) -> List.of());

    IllegalStateException failure = assertThrows(IllegalStateException.class, builder::build);

    assertEquals("the devtools artifact id is required", failure.getMessage());
  }

  @Test
  void shouldResolveTheClosureOfTheCoreDevtools() throws Exception {
    ApplicationClasspath application = new ApplicationClasspath(
        List.of(jar(GROUP_ID, FOUNDATION_ARTIFACT, VERSION, FOUNDATION_JAR)));
    List<String> asked = new ArrayList<>();
    ArtifactResolver resolver = (groupId, artifactId, version) -> {
      asked.add(artifactId);
      if (ApplicationClasspath.FRAMEWORK_ARTIFACT_ID.equals(artifactId)) {
        return List.of(jar(GROUP_ID, FOUNDATION_ARTIFACT, VERSION, CARRIED_JAR));
      }

      return List.of(jar(GROUP_ID, DEVTOOLS_ARTIFACT, VERSION, "webforj-devtools.jar"),
          jar(GROUP_ID, FOUNDATION_ARTIFACT, VERSION, CARRIED_JAR),
          jar(WEBSOCKET_GROUP_ID, WEBSOCKET_ARTIFACT, WEBSOCKET_VERSION, WEBSOCKET_JAR));
    };

    List<ResolvedJar> additions = DevtoolsDelivery.create().setArtifactId(DEVTOOLS_ARTIFACT)
        .setApplicationClasspath(application).setResolver(resolver).build().getJars();

    assertEquals(List.of(ApplicationClasspath.FRAMEWORK_ARTIFACT_ID, DEVTOOLS_ARTIFACT), asked);
    assertEquals(
        List.of(jar(GROUP_ID, DEVTOOLS_ARTIFACT, VERSION, "webforj-devtools.jar"),
            jar(WEBSOCKET_GROUP_ID, WEBSOCKET_ARTIFACT, WEBSOCKET_VERSION, WEBSOCKET_JAR)),
        additions);
  }

  private static ResolvedJar jar(String groupId, String artifactId, String version,
      String fileName) {
    return new ResolvedJar(groupId, artifactId, version, Path.of(fileName));
  }
}
