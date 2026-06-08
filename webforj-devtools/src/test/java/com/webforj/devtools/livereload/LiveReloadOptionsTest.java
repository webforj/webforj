package com.webforj.devtools.livereload;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import org.junit.jupiter.api.Test;

class LiveReloadOptionsTest {

  @Test
  void shouldCarryTheDefaults() {
    LiveReloadOptions options = new LiveReloadOptions();

    assertFalse(options.isEnabled());
    assertEquals(35730, options.getWebsocketPort());
    assertEquals("/webforj-devtools-ws", options.getWebsocketPath());
    assertEquals(30000, options.getHeartbeatInterval());
    assertTrue(options.isStaticResourcesEnabled());
  }

  @Test
  void shouldReadValuesFromConfig() {
    Config config = ConfigFactory.parseString("webforj.devtools.livereload {"
        + " enabled = true, websocket-port = 4000, websocket-path = \"/x\","
        + " heartbeat-interval = 5000, static-resources-enabled = false }");

    LiveReloadOptions options = LiveReloadOptions.from(config);

    assertTrue(options.isEnabled());
    assertEquals(4000, options.getWebsocketPort());
    assertEquals("/x", options.getWebsocketPath());
    assertEquals(5000, options.getHeartbeatInterval());
    assertFalse(options.isStaticResourcesEnabled());
  }

  @Test
  void shouldFallBackToDefaultsWhenKeysAbsent() {
    LiveReloadOptions options = LiveReloadOptions.from(ConfigFactory.empty());

    assertFalse(options.isEnabled());
    assertEquals(35730, options.getWebsocketPort());
    assertEquals("/webforj-devtools-ws", options.getWebsocketPath());
    assertEquals(30000, options.getHeartbeatInterval());
    assertTrue(options.isStaticResourcesEnabled());
  }
}
