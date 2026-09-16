package com.webforj.spring.devtools.livereload;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import com.webforj.devtools.livereload.LiveReloadLifecycle;
import java.io.File;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.boot.devtools.classpath.ClassPathChangedEvent;
import org.springframework.boot.devtools.filewatch.ChangedFile;
import org.springframework.boot.devtools.filewatch.ChangedFiles;

class LiveReloadResourceChangeListenerTest {

  @Test
  void shouldSendAStylesheetChangeAsAResourceUpdate() {
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);

    new LiveReloadResourceChangeListener(lifecycle)
        .onApplicationEvent(event("static/app.css", ChangedFile.Type.MODIFY));

    verify(lifecycle).sendResourceUpdate("css", "app.css");
  }

  @Test
  void shouldKeepTheNestedPathUnderTheStaticDirectory() {
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);

    new LiveReloadResourceChangeListener(lifecycle)
        .onApplicationEvent(event("static/img/logo.svg", ChangedFile.Type.MODIFY));

    verify(lifecycle).sendResourceUpdate("image", "img/logo.svg");
  }

  @Test
  void shouldClassifyUnknownExtensionsAsOther() {
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);

    new LiveReloadResourceChangeListener(lifecycle)
        .onApplicationEvent(event("static/data.json", ChangedFile.Type.ADD));

    verify(lifecycle).sendResourceUpdate("other", "data.json");
  }

  @ParameterizedTest
  @ValueSource(strings = {"static/frontend/app.css", "static/webforj/frontend-entries.json",
      "com/example/Application.class"})
  void shouldIgnoreExcludedResourceChanges(String relativePath) {
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);

    new LiveReloadResourceChangeListener(lifecycle)
        .onApplicationEvent(event(relativePath, ChangedFile.Type.MODIFY));

    verifyNoInteractions(lifecycle);
  }

  private static ClassPathChangedEvent event(String relativePath, ChangedFile.Type type) {
    File sourceDirectory = new File("/project/target/classes");
    ChangedFile file =
        new ChangedFile(sourceDirectory, new File(sourceDirectory, relativePath), type);

    return new ClassPathChangedEvent(new Object(),
        Set.of(new ChangedFiles(sourceDirectory, Set.of(file))), false);
  }
}
