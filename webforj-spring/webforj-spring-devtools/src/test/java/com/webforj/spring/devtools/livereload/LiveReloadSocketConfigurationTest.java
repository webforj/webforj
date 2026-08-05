package com.webforj.spring.devtools.livereload;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import com.webforj.devtools.livereload.LiveReloadLifecycle;
import com.webforj.devtools.livereload.LiveReloadOptions;
import com.webforj.spring.SpringConfigurationProperties;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.boot.test.context.FilteredClassLoader;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
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
  void shouldRegisterTheLifecycleWithoutSpringDevtoolsOnTheClasspath() {
    SpringConfigurationProperties properties = new SpringConfigurationProperties();
    properties.getDevtools().getLivereload().setWebsocketPort(46101);

    new ApplicationContextRunner()
        .withClassLoader(new FilteredClassLoader("org.springframework.boot.devtools"))
        .withPropertyValues("webforj.devtools.livereload.enabled=true")
        .withBean(SpringConfigurationProperties.class, () -> properties)
        .withUserConfiguration(LiveReloadSocketConfiguration.class).run(context -> {
          assertNotNull(context.getBean(LiveReloadLifecycle.class));
          assertNotNull(context.getBean(LiveReloadListener.class));
          assertNotNull(context.getBean(LiveReloadRestartListener.class));
          assertFalse(context.containsBean("liveReloadResourceChangeListener"));
        });
  }

  @Test
  void shouldRegisterTheResourceChangeListenerWithSpringDevtoolsOnTheClasspath() {
    SpringConfigurationProperties properties = new SpringConfigurationProperties();
    properties.getDevtools().getLivereload().setWebsocketPort(46102);

    new ApplicationContextRunner().withPropertyValues("webforj.devtools.livereload.enabled=true")
        .withBean(SpringConfigurationProperties.class, () -> properties)
        .withUserConfiguration(LiveReloadSocketConfiguration.class)
        .run(context -> assertNotNull(context.getBean(LiveReloadResourceChangeListener.class)));
  }

  @Test
  void shouldRegisterNothingWhenLiveReloadIsOff() {
    new ApplicationContextRunner()
        .withBean(SpringConfigurationProperties.class, SpringConfigurationProperties::new)
        .withUserConfiguration(LiveReloadSocketConfiguration.class)
        .run(context -> assertFalse(context.containsBean("liveReloadLifecycle")));
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
