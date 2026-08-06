package com.webforj.plugin.gradle.devtools;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import com.webforj.plugin.gradle.WebforjPlugin;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicInteger;
import org.gradle.api.Project;
import org.gradle.api.file.FileCollection;
import org.gradle.testfixtures.ProjectBuilder;
import org.junit.jupiter.api.Test;

class SpringDevtoolsInjectionTest {

  @Test
  void shouldResolveOnlyOnceThroughTheMemoizedCallable() throws Exception {
    AtomicInteger calls = new AtomicInteger();
    FileCollection files = mock(FileCollection.class);

    Callable<FileCollection> memoized = SpringDevtoolsInjection.memoize(() -> {
      calls.incrementAndGet();
      return files;
    });

    assertEquals(files, memoized.call());
    assertEquals(files, memoized.call());
    assertEquals(1, calls.get(),
        "the task classpath asks more than once, the resolution runs once");
  }

  @Test
  void shouldDeliverNothingToTheApplicationWithoutTheFramework() {
    Project project = ProjectBuilder.builder().build();
    project.getPlugins().apply("java");
    project.getPlugins().apply(WebforjPlugin.class);

    FileCollection resolved = SpringDevtoolsInjection.resolve(project);

    assertTrue(resolved.isEmpty(),
        "an application without webforJ on its classpath receives no devtools");
  }
}
