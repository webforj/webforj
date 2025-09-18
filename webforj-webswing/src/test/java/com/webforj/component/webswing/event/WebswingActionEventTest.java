package com.webforj.component.webswing.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import com.webforj.component.webswing.WebswingConnector;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class WebswingActionEventTest {

  private WebswingConnector mockConnector;
  private Map<String, Object> eventMap;

  @BeforeEach
  void setUp() {
    mockConnector = mock(WebswingConnector.class);
    eventMap = new HashMap<>();
  }

  @Test
  void shouldCreateEventWithActionNameOnly() {
    String actionName = "submit";
    eventMap.put("actionName", actionName);
    eventMap.put("data", null);
    eventMap.put("binaryDataBase64", null);

    WebswingActionEvent event = new WebswingActionEvent(mockConnector, eventMap);

    assertEquals(actionName, event.getActionName());
    assertFalse(event.getActionData().isPresent());
    assertFalse(event.getActionBinaryData().isPresent());
    assertSame(mockConnector, event.getComponent());
  }

  @Test
  void shouldCreateEventWithActionNameAndData() {
    String actionName = "submit";
    String data = "{\"test\":\"value\"}";
    eventMap.put("actionName", actionName);
    eventMap.put("data", data);
    eventMap.put("binaryDataBase64", null);

    WebswingActionEvent event = new WebswingActionEvent(mockConnector, eventMap);

    assertEquals(actionName, event.getActionName());
    assertTrue(event.getActionData().isPresent());
    assertEquals(data, event.getActionData().get());
    assertFalse(event.getActionBinaryData().isPresent());
  }

  @Test
  void shouldCreateEventWithAllProperties() {
    String actionName = "submit";
    String data = "{\"test\":\"value\"}";
    String originalBinaryData = "Hello Binary World";
    String binaryDataBase64 =
        Base64.getEncoder().encodeToString(originalBinaryData.getBytes(StandardCharsets.UTF_8));

    eventMap.put("actionName", actionName);
    eventMap.put("data", data);
    eventMap.put("binaryDataBase64", binaryDataBase64);

    WebswingActionEvent event = new WebswingActionEvent(mockConnector, eventMap);

    assertEquals(actionName, event.getActionName());
    assertTrue(event.getActionData().isPresent());
    assertEquals(data, event.getActionData().get());
    assertTrue(event.getActionBinaryData().isPresent());
    assertEquals(originalBinaryData, event.getActionBinaryData().get());
  }
}
