package com.webforj.devtools.livereload;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.typesafe.config.ConfigFactory;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.devtools.ProjectRootResolver;
import com.webforj.devtools.livereload.LiveReloadBBjLifecycle.SharedLifecycle;
import java.io.IOException;
import java.net.ServerSocket;
import java.nio.file.Path;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;
import org.mockito.MockedStatic;

class LiveReloadBBjLifecycleTest {

  @Test
  @Timeout(10)
  void shouldRunFromTheFirstSessionToTheLast() throws IOException {
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();
    SharedLifecycle shared = new SharedLifecycle(lifecycle);
    LiveReloadBBjLifecycle first = new LiveReloadBBjLifecycle(shared);
    LiveReloadBBjLifecycle second = new LiveReloadBBjLifecycle(shared);
    LiveReloadOptions options = enabledOptions();

    try {
      first.start(options);
      second.start(options);
      assertTrue(lifecycle.isRunning(), "the first session should start live reload");

      first.stop();
      assertTrue(lifecycle.isRunning(),
          "live reload should outlive a session that is not the last");

      second.stop();
      assertFalse(lifecycle.isRunning(), "the last session should stop live reload");
    } finally {
      lifecycle.stop();
    }
  }

  @Test
  @Timeout(10)
  void shouldCountOneSessionOnce() throws IOException {
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();
    SharedLifecycle shared = new SharedLifecycle(lifecycle);
    LiveReloadBBjLifecycle first = new LiveReloadBBjLifecycle(shared);
    LiveReloadBBjLifecycle second = new LiveReloadBBjLifecycle(shared);
    LiveReloadOptions options = enabledOptions();

    try {
      first.start(options);
      first.start(options);
      second.start(options);

      first.stop();
      first.stop();
      assertTrue(lifecycle.isRunning(), "a repeated stop should not end the other session");
    } finally {
      lifecycle.stop();
    }
  }

  @Test
  @Timeout(10)
  void shouldNotCountSessionWithLiveReloadDisabled() throws IOException {
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();
    SharedLifecycle shared = new SharedLifecycle(lifecycle);
    LiveReloadBBjLifecycle running = new LiveReloadBBjLifecycle(shared);
    LiveReloadBBjLifecycle disabled = new LiveReloadBBjLifecycle(shared);

    try {
      running.start(enabledOptions());
      disabled.start(new LiveReloadOptions().setEnabled(false));
      disabled.stop();

      assertTrue(lifecycle.isRunning(), "a disabled session should not end the running one");
    } finally {
      lifecycle.stop();
    }
  }

  @Test
  void shouldStartWithTheProjectRootAndContentUpdatesInBbjServices(@TempDir Path root) {
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);
    LiveReloadBBjLifecycle listener = new LiveReloadBBjLifecycle(new SharedLifecycle(lifecycle));
    App app = mock(App.class);
    Environment environment = mock(Environment.class);
    when(environment.getConfig())
        .thenReturn(ConfigFactory.parseMap(Map.of(LiveReloadOptions.KEY_ENABLED, true,
            ProjectRootResolver.KEY_PROJECT_ROOT, root.toString())));

    try (MockedStatic<Environment> environments = mockStatic(Environment.class)) {
      environments.when(Environment::isRunningWithBBjServices).thenReturn(true);
      environments.when(Environment::getCurrent).thenReturn(environment);

      listener.onWillRun(app);
      listener.onWillTerminate(app);
    }

    ArgumentCaptor<LiveReloadOptions> options = ArgumentCaptor.forClass(LiveReloadOptions.class);
    verify(lifecycle).start(options.capture());
    assertEquals(root.toAbsolutePath().normalize().toString(), options.getValue().getProjectRoot());
    assertFalse(options.getValue().isStaticResourcesEnabled());
    assertTrue(options.getValue().isStopOnWatchEnd());
    verify(lifecycle).stop();
  }

  @Test
  void shouldKeepContentUpdatesWithPackagedAssetsInBbjServices(@TempDir Path root) {
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);
    LiveReloadBBjLifecycle listener = new LiveReloadBBjLifecycle(new SharedLifecycle(lifecycle));
    App app = mock(App.class);
    Environment environment = mock(Environment.class);
    when(environment.getConfig()).thenReturn(ConfigFactory
        .parseMap(Map.of(LiveReloadOptions.KEY_ENABLED, true, ProjectRootResolver.KEY_PROJECT_ROOT,
            root.toString(), "webforj.assetsSource", "packaged")));

    try (MockedStatic<Environment> environments = mockStatic(Environment.class)) {
      environments.when(Environment::isRunningWithBBjServices).thenReturn(true);
      environments.when(Environment::getCurrent).thenReturn(environment);

      listener.onWillRun(app);
      listener.onWillTerminate(app);
    }

    ArgumentCaptor<LiveReloadOptions> options = ArgumentCaptor.forClass(LiveReloadOptions.class);
    verify(lifecycle).start(options.capture());
    assertTrue(options.getValue().isStaticResourcesEnabled());
    assertTrue(options.getValue().isStopOnWatchEnd());
  }

  @Test
  void shouldStayOffOutsideBbjServices() {
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);
    LiveReloadBBjLifecycle listener = new LiveReloadBBjLifecycle(new SharedLifecycle(lifecycle));
    App app = mock(App.class);
    Environment environment = mock(Environment.class);
    when(environment.getConfig())
        .thenReturn(ConfigFactory.parseMap(Map.of(LiveReloadOptions.KEY_ENABLED, true)));

    try (MockedStatic<Environment> environments = mockStatic(Environment.class)) {
      environments.when(Environment::isRunningWithBBjServices).thenReturn(false);
      environments.when(Environment::getCurrent).thenReturn(environment);

      listener.onWillRun(app);
      listener.onWillTerminate(app);
    }

    verify(lifecycle, never()).start(any());
    verify(lifecycle, never()).stop();
  }

  @Test
  void shouldStayOffWithoutLiveReloadConfiguration() {
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);
    LiveReloadBBjLifecycle listener = new LiveReloadBBjLifecycle(new SharedLifecycle(lifecycle));
    App app = mock(App.class);

    try (MockedStatic<Environment> environments = mockStatic(Environment.class)) {
      environments.when(Environment::isRunningWithBBjServices).thenReturn(true);
      environments.when(Environment::getCurrent).thenReturn(null);

      listener.onWillRun(app);
      listener.onWillTerminate(app);
    }

    verify(lifecycle, never()).start(any());
    verify(lifecycle, never()).stop();
  }

  @Test
  void shouldOpenTheClassLoaderForTheFirstSessionAndStopAfterTheLast() {
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);
    when(lifecycle.isRunning()).thenReturn(false, true);
    SharedLifecycle shared = new SharedLifecycle(lifecycle);
    LiveReloadBBjLifecycle first = new LiveReloadBBjLifecycle(shared);
    LiveReloadBBjLifecycle second = new LiveReloadBBjLifecycle(shared);
    LiveReloadOptions options = new LiveReloadOptions().setEnabled(true);

    first.start(options);
    second.start(options);
    first.stop();
    second.stop();

    InOrder order = inOrder(lifecycle);
    order.verify(lifecycle).start(options);
    ClassLoader loader = LiveReloadBBjLifecycle.class.getClassLoader();
    order.verify(lifecycle).openClassLoader(loader);
    order.verify(lifecycle).stop();
    verify(lifecycle, times(1)).openClassLoader(loader);
    verify(lifecycle, times(1)).stop();
  }

  @Test
  void shouldStartAgainForSessionAfterTheRunEnded() {
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);
    when(lifecycle.isRunning()).thenReturn(false, false);
    SharedLifecycle shared = new SharedLifecycle(lifecycle);
    LiveReloadBBjLifecycle first = new LiveReloadBBjLifecycle(shared);
    LiveReloadBBjLifecycle second = new LiveReloadBBjLifecycle(shared);
    LiveReloadOptions options = new LiveReloadOptions().setEnabled(true);

    first.start(options);
    second.start(options);

    verify(lifecycle, times(2)).start(options);
    ClassLoader loader = LiveReloadBBjLifecycle.class.getClassLoader();
    verify(lifecycle, times(2)).openClassLoader(loader);
  }

  @Test
  void shouldClearTheSessionBeforeCleanup() {
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);
    when(lifecycle.isRunning()).thenReturn(false);
    doThrow(new IllegalStateException("stop failed")).doNothing().when(lifecycle).stop();
    LiveReloadBBjLifecycle listener = new LiveReloadBBjLifecycle(new SharedLifecycle(lifecycle));
    LiveReloadOptions options = new LiveReloadOptions().setEnabled(true);
    listener.start(options);

    assertThrows(IllegalStateException.class, () -> listener.stop());

    listener.start(options);
    listener.stop();

    verify(lifecycle, times(2)).start(options);
    verify(lifecycle, times(2)).stop();
  }

  private static LiveReloadOptions enabledOptions() throws IOException {
    try (ServerSocket socket = new ServerSocket(0)) {
      return new LiveReloadOptions().setEnabled(true).setWebsocketPort(socket.getLocalPort());
    }
  }
}
