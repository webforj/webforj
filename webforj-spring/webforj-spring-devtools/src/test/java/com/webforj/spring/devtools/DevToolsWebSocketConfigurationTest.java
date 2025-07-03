package com.webforj.spring.devtools;

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

class DevToolsWebSocketConfigurationTest {

  private ApplicationContextRunner contextRunner;
  private int availablePort;

  @BeforeEach
  void setUp() throws IOException {
    availablePort = findAvailablePort();
    contextRunner =
        new ApplicationContextRunner().withUserConfiguration(DevToolsWebSocketConfiguration.class)
            .withPropertyValues("webforj.devtools.reload.websocket-port=" + availablePort);
    DevToolsState.setWebSocketServer(null);
    DevToolsState.markStarted();
  }

  @AfterEach
  void tearDown() throws InterruptedException {
    DevToolsServer server = DevToolsState.getWebSocketServer();
    if (server != null && server.isOpen()) {
      server.stop();
    }
    DevToolsState.setWebSocketServer(null);
  }

  private int findAvailablePort() throws IOException {
    try (ServerSocket socket = new ServerSocket(0)) {
      return socket.getLocalPort();
    }
  }

  @Test
  void shouldCreateBeansWhenEnabledAndDevToolsPresent() {
    contextRunner.withPropertyValues("webforj.devtools.reload.enabled=true").run(context -> {
      assertNotNull(context.getBean(DevToolsReloadService.class));
      assertNotNull(context.getBean(DevToolsReloadListener.class));
      assertNotNull(context.getBean(DevToolsResourceChangeListener.class));
      assertNotNull(context.getBean(DevToolsReloadProperties.class));
    });
  }

  @Test
  void shouldNotCreateBeansWhenDisabled() {
    contextRunner.withPropertyValues("webforj.devtools.reload.enabled=false").run(context -> {
      assertNull(
          context.getBeanNamesForType(DevToolsReloadService.class).length == 0 ? null : "found");
      assertNull(
          context.getBeanNamesForType(DevToolsReloadListener.class).length == 0 ? null : "found");
      assertNull(
          context.getBeanNamesForType(DevToolsResourceChangeListener.class).length == 0 ? null
              : "found");
    });
  }

  @Test
  void shouldNotCreateBeansWhenNotConfigured() {
    contextRunner.run(context -> {
      assertEquals(0, context.getBeanNamesForType(DevToolsReloadService.class).length);
      assertEquals(0, context.getBeanNamesForType(DevToolsReloadListener.class).length);
      assertEquals(0, context.getBeanNamesForType(DevToolsResourceChangeListener.class).length);
    });
  }

  @Test
  void shouldCreateResourceListenerWhenStaticResourcesEnabled() {
    contextRunner.withPropertyValues("webforj.devtools.reload.enabled=true",
        "webforj.devtools.reload.static-resources-enabled=true").run(context -> {
          assertNotNull(context.getBean(DevToolsResourceChangeListener.class));
        });
  }

  @Test
  void shouldNotCreateResourceListenerWhenStaticResourcesDisabled() {
    contextRunner.withPropertyValues("webforj.devtools.reload.enabled=true",
        "webforj.devtools.reload.static-resources-enabled=false").run(context -> {
          assertEquals(0, context.getBeanNamesForType(DevToolsResourceChangeListener.class).length);
          assertNotNull(context.getBean(DevToolsReloadService.class));
          assertNotNull(context.getBean(DevToolsReloadListener.class));
        });
  }

  @Test
  void shouldCreateResourceListenerByDefaultWhenMainFeatureEnabled() {
    contextRunner.withPropertyValues("webforj.devtools.reload.enabled=true").run(context -> {
      assertNotNull(context.getBean(DevToolsResourceChangeListener.class));
    });
  }

  @Test
  void shouldConfigureWebSocketPortFromProperties() throws IOException {
    int customPort = findAvailablePort();
    contextRunner.withPropertyValues("webforj.devtools.reload.enabled=true",
        "webforj.devtools.reload.websocket-port=" + customPort).run(context -> {
          DevToolsReloadProperties properties = context.getBean(DevToolsReloadProperties.class);
          assertEquals(customPort, properties.getWebsocketPort());
        });
  }

  @Test
  void shouldUseConfiguredPortWhenProvided() {
    contextRunner.withPropertyValues("webforj.devtools.reload.enabled=true").run(context -> {
      DevToolsReloadProperties properties = context.getBean(DevToolsReloadProperties.class);
      assertEquals(availablePort, properties.getWebsocketPort());
    });
  }

  @Test
  void shouldReuseExistingWebSocketServerOnRestart() {
    contextRunner.withPropertyValues("webforj.devtools.reload.enabled=true")
        .withUserConfiguration(MockWebSocketServerConfiguration.class).run(context -> {
          DevToolsServer existingServer = DevToolsState.getWebSocketServer();
          assertNotNull(existingServer);
          assertTrue(existingServer.isOpen());
          DevToolsReloadService service = context.getBean(DevToolsReloadService.class);
          assertNotNull(service);
        });
  }

  @Configuration
  static class MockWebSocketServerConfiguration {
    @Bean
    String mockServerSetup() {
      DevToolsServer mockServer = mock(DevToolsServer.class);
      when(mockServer.isOpen()).thenReturn(true);
      when(mockServer.getPort()).thenReturn(12345);
      when(mockServer.getConnectionCount()).thenReturn(2);
      DevToolsState.setWebSocketServer(mockServer);
      return "configured";
    }
  }
}
