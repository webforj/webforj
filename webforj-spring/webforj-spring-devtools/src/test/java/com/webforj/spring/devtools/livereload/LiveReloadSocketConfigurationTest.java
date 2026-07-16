package com.webforj.spring.devtools.livereload;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import com.webforj.devtools.livereload.LiveReloadLifecycle;
import com.webforj.devtools.livereload.LiveReloadOptions;
import com.webforj.spring.SpringConfigurationProperties;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.context.event.ContextRefreshedEvent;

class LiveReloadSocketConfigurationTest {

  @Test
  void shouldProvideALifecycle() {
    assertNotNull(new LiveReloadSocketConfiguration().liveReloadLifecycle());
  }

  @Test
  void shouldBuildAListenerFromTheBoundProperties() {
    SpringConfigurationProperties properties = new SpringConfigurationProperties();
    properties.getDevtools().getLivereload().setWebsocketPort(40000);
    properties.getDevtools().getLivereload().setStaticResourcesEnabled(false);
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);

    new LiveReloadSocketConfiguration().liveReloadListener(properties, lifecycle)
        .onApplicationEvent(mock(ContextRefreshedEvent.class));

    ArgumentCaptor<LiveReloadOptions> captor = ArgumentCaptor.forClass(LiveReloadOptions.class);
    verify(lifecycle).start(captor.capture());
    assertTrue(captor.getValue().isEnabled());
    assertEquals(40000, captor.getValue().getWebsocketPort());
    assertEquals(false, captor.getValue().isStaticResourcesEnabled());
  }

  @Test
  void shouldBuildARestartListenerBoundToTheLifecycle() {
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);

    new LiveReloadSocketConfiguration().liveReloadRestartListener(lifecycle)
        .onApplicationEvent(mock(ContextClosedEvent.class));

    verify(lifecycle).notifyRestarting();
  }

  @Test
  void shouldBuildAResourceChangeListenerBoundToTheLifecycle() {
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);

    assertNotNull(new LiveReloadSocketConfiguration().liveReloadResourceChangeListener(lifecycle));
  }

  @Test
  void shouldFallBackToOptionDefaultsWhenPropertiesAreUnset() {
    SpringConfigurationProperties properties = new SpringConfigurationProperties();
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);

    new LiveReloadSocketConfiguration().liveReloadListener(properties, lifecycle)
        .onApplicationEvent(mock(ContextRefreshedEvent.class));

    ArgumentCaptor<LiveReloadOptions> captor = ArgumentCaptor.forClass(LiveReloadOptions.class);
    verify(lifecycle).start(captor.capture());
    assertEquals(LiveReloadOptions.DEFAULT_WEBSOCKET_PORT, captor.getValue().getWebsocketPort());
    assertTrue(captor.getValue().isStaticResourcesEnabled());
  }
}
