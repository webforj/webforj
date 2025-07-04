package com.webforj.spring.devtools.livereload;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.net.ServerSocket;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

class LiveReloadSocketConfigurationTest {

  private ApplicationContextRunner contextRunner;
  private int availablePort;

  @BeforeEach
  void setUp() throws IOException {
    availablePort = findAvailablePort();
    contextRunner =
        new ApplicationContextRunner().withUserConfiguration(LiveReloadSocketConfiguration.class)
            .withPropertyValues("webforj.devtools.livereload.websocket-port=" + availablePort);
    LiveReloadState.setWebSocketServer(null);
    LiveReloadState.markStarted();
  }

  @AfterEach
  void tearDown() throws InterruptedException {
    LiveReloadServer server = LiveReloadState.getWebSocketServer();
    if (server != null && server.isOpen()) {
      server.stop();
    }
    LiveReloadState.setWebSocketServer(null);
  }

  private int findAvailablePort() throws IOException {
    try (ServerSocket socket = new ServerSocket(0)) {
      return socket.getLocalPort();
    }
  }

  @Test
  void shouldCreateBeansWhenEnabledAndDevToolsPresent() {
    contextRunner.withPropertyValues("webforj.devtools.livereload.enabled=true").run(context -> {
      assertNotNull(context.getBean(LiveReloadService.class));
      assertNotNull(context.getBean(LiveReloadListener.class));
      assertNotNull(context.getBean(LiveReloadResourceChangeListener.class));
      assertNotNull(context.getBean(LiveReloadProperties.class));
    });
  }

  @Test
  void shouldNotCreateBeansWhenDisabled() {
    contextRunner.withPropertyValues("webforj.devtools.livereload.enabled=false").run(context -> {
      assertNull(context.getBeanNamesForType(LiveReloadService.class).length == 0 ? null : "found");
      assertNull(
          context.getBeanNamesForType(LiveReloadListener.class).length == 0 ? null : "found");
      assertNull(
          context.getBeanNamesForType(LiveReloadResourceChangeListener.class).length == 0 ? null
              : "found");
    });
  }

  @Test
  void shouldNotCreateBeansWhenNotConfigured() {
    contextRunner.run(context -> {
      assertEquals(0, context.getBeanNamesForType(LiveReloadService.class).length);
      assertEquals(0, context.getBeanNamesForType(LiveReloadListener.class).length);
      assertEquals(0, context.getBeanNamesForType(LiveReloadResourceChangeListener.class).length);
    });
  }

  @Test
  void shouldCreateResourceListenerWhenStaticResourcesEnabled() {
    contextRunner.withPropertyValues("webforj.devtools.livereload.enabled=true",
        "webforj.devtools.livereload.static-resources-enabled=true").run(context -> {
          assertNotNull(context.getBean(LiveReloadResourceChangeListener.class));
        });
  }

  @Test
  void shouldNotCreateResourceListenerWhenStaticResourcesDisabled() {
    contextRunner.withPropertyValues("webforj.devtools.livereload.enabled=true",
        "webforj.devtools.livereload.static-resources-enabled=false").run(context -> {
          assertEquals(0,
              context.getBeanNamesForType(LiveReloadResourceChangeListener.class).length);
          assertNotNull(context.getBean(LiveReloadService.class));
          assertNotNull(context.getBean(LiveReloadListener.class));
        });
  }

  @Test
  void shouldCreateResourceListenerByDefaultWhenMainFeatureEnabled() {
    contextRunner.withPropertyValues("webforj.devtools.livereload.enabled=true").run(context -> {
      assertNotNull(context.getBean(LiveReloadResourceChangeListener.class));
    });
  }

  @Test
  void shouldConfigureWebSocketPortFromProperties() throws IOException {
    int customPort = findAvailablePort();
    contextRunner.withPropertyValues("webforj.devtools.livereload.enabled=true",
        "webforj.devtools.livereload.websocket-port=" + customPort).run(context -> {
          LiveReloadProperties properties = context.getBean(LiveReloadProperties.class);
          assertEquals(customPort, properties.getWebsocketPort());
        });
  }

  @Test
  void shouldUseConfiguredPortWhenProvided() {
    contextRunner.withPropertyValues("webforj.devtools.livereload.enabled=true").run(context -> {
      LiveReloadProperties properties = context.getBean(LiveReloadProperties.class);
      assertEquals(availablePort, properties.getWebsocketPort());
    });
  }

  @Test
  void shouldReuseExistingWebSocketServerOnRestart() {
    contextRunner.withPropertyValues("webforj.devtools.livereload.enabled=true")
        .withUserConfiguration(MockWebSocketServerConfiguration.class).run(context -> {
          LiveReloadServer existingServer = LiveReloadState.getWebSocketServer();
          assertNotNull(existingServer);
          assertTrue(existingServer.isOpen());
          LiveReloadService service = context.getBean(LiveReloadService.class);
          assertNotNull(service);
        });
  }

  @Configuration
  static class MockWebSocketServerConfiguration {
    @Bean
    String mockServerSetup() {
      LiveReloadServer mockServer = mock(LiveReloadServer.class);
      when(mockServer.isOpen()).thenReturn(true);
      when(mockServer.getPort()).thenReturn(12345);
      when(mockServer.getConnectionCount()).thenReturn(2);
      LiveReloadState.setWebSocketServer(mockServer);
      return "configured";
    }
  }
}
