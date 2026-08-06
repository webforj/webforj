package com.webforj.plugin.foundation.hotswap.hotswapagent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.plugin.foundation.resolve.ApplicationClasspath;
import com.webforj.plugin.foundation.resolve.ApplicationClasspath.ResolvedJar;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;

class HotswapObserverJarTest {

  private static final String VERSION = "26.02-SNAPSHOT";

  @Test
  void shouldResolveTheObserverAtTheFrameworkVersionOfTheApplication() throws Exception {
    Path observer = Path.of("webforj-hotswap-observer.jar");
    List<String> versions = new ArrayList<>();

    Path resolved =
        HotswapObserverJar.resolve(classpathWithFramework(), (groupId, artifactId, version) -> {
          versions.add(version);
          return List.of(new ResolvedJar(groupId, artifactId, version, observer));
        });

    assertEquals(observer, resolved);
    assertEquals(List.of(VERSION), versions,
        "the observer resolves at the framework version of the application");
  }

  @Test
  void shouldFailWhenTheApplicationHasNoFramework() {
    ApplicationClasspath classpath = new ApplicationClasspath(
        List.of(new ResolvedJar("org.springframework", "spring-core", "6.2.0", null)));

    IOException failure = assertThrows(IOException.class,
        () -> HotswapObserverJar.resolve(classpath, (groupId, artifactId, version) -> List.of()));

    assertTrue(failure.getMessage().contains(ApplicationClasspath.FRAMEWORK_ARTIFACT_ID),
        "the missing requirement is named");
  }

  @Test
  void shouldFailWhenTheResolutionCarriesNoObserver() {
    IOException failure = assertThrows(IOException.class,
        () -> HotswapObserverJar.resolve(classpathWithFramework(),
            (groupId, artifactId, version) -> List
                .of(new ResolvedJar("org.example", "other", version, Path.of("other.jar")))));

    assertTrue(failure.getMessage().contains(HotswapObserverJar.ARTIFACT_ID),
        "the missing artifact is named");
  }

  private static ApplicationClasspath classpathWithFramework() {
    return new ApplicationClasspath(List.of(new ResolvedJar(ApplicationClasspath.FRAMEWORK_GROUP_ID,
        ApplicationClasspath.FRAMEWORK_ARTIFACT_ID, VERSION, Path.of("webforj-foundation.jar"))));
  }
}
