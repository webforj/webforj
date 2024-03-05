package com.webforj.component.html.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import com.webforj.component.html.HtmlComponent;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HtmlClickEventTest {

  private HtmlClickEvent clickEvent;

  @BeforeEach
  void setUp() {
    Map<String, Object> eventMap = new HashMap<>();
    eventMap.put("screenX", 100);
    eventMap.put("screenY", 200);
    eventMap.put("clientX", 50);
    eventMap.put("clientY", 100);
    eventMap.put("detail", 1);
    eventMap.put("button", 1);
    eventMap.put("ctrlKey", true);
    eventMap.put("shiftKey", false);
    eventMap.put("altKey", true);
    eventMap.put("metaKey", false);

    clickEvent = new HtmlClickEvent(mock(HtmlComponent.class), eventMap);
  }

  @Test
  void testGetScreenX() {
    assertEquals(100, clickEvent.getScreenX());
  }

  @Test
  void testGetScreenY() {
    assertEquals(200, clickEvent.getScreenY());
  }

  @Test
  void testGetClientX() {
    assertEquals(50, clickEvent.getClientX());
  }

  @Test
  void testGetClientY() {
    assertEquals(100, clickEvent.getClientY());
  }

  @Test
  void testGetDetail() {
    assertEquals(1, clickEvent.getDetail());
  }

  @Test
  void testGetButton() {
    assertEquals(1, clickEvent.getButton());
  }

  @Test
  void testIsCtrlKey() {
    assertTrue(clickEvent.isCtrlKey());
  }

  @Test
  void testIsShiftKey() {
    assertFalse(clickEvent.isShiftKey());
  }

  @Test
  void testIsAltKey() {
    assertTrue(clickEvent.isAltKey());
  }

  @Test
  void testIsMetaKey() {
    assertFalse(clickEvent.isMetaKey());
  }
}
