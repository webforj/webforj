package com.webforj.environment.namespace;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjNamespace;
import com.basis.startup.type.BBjException;
import com.webforj.Environment;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.environment.namespace.event.NamespaceEvent;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class NamespaceEventTest {

  Namespace namespace;
  NamespaceEvent event;

  @BeforeEach
  void setUp() throws BBjException {
    BBjAPI api = mock(BBjAPI.class);
    WebforjBBjBridge bridge = mock(WebforjBBjBridge.class);
    Environment.init(api, bridge, 0);

    namespace = new GlobalNamespace();
    namespace.setBbjNamespace(mock(BBjNamespace.class));

    event = new NamespaceEvent(namespace, "variable", "oldValue", "newValue") {};
  }

  @AfterEach
  void tearDown() {
    Environment.cleanup();
  }

  @Test
  void shouldReturnNamespace() {
    assertEquals(namespace, event.getNamespace());
  }

  @Test
  void shouldReturnVariableName() {
    assertEquals("variable", event.getVariableName());
  }

  @Test
  void shouldReturnOldValue() {
    assertEquals("oldValue", event.getOldValue());
  }

  @Test
  void shouldReturnNewValue() {
    assertEquals("newValue", event.getNewValue());
  }

  @Test
  void shouldReturnStringRepresentation() {
    String expected = "Namespace Event: namespace=" + namespace.getName()
        + " variable=variable old=oldValue new= newValue";
    assertNotNull(event.toString());
    assertEquals(expected, event.toString());
  }
}
