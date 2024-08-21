package com.webforj.component.element.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import com.webforj.component.element.ElementComposite;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class ElementClickEventTest {

  private ElementClickEvent<?> clickEvent;

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

    clickEvent = new ElementClickEvent(mock(ElementComposite.class), eventMap);
  }

  @Test
  void shouldGetScreenX() {
    assertEquals(100, clickEvent.getScreenX());
  }

  @Test
  void shouldGetScreenY() {
    assertEquals(200, clickEvent.getScreenY());
  }

  @Test
  void shouldGetClientX() {
    assertEquals(50, clickEvent.getClientX());
  }

  @Test
  void shouldGetClientY() {
    assertEquals(100, clickEvent.getClientY());
  }

  @Test
  void shouldGetDetail() {
    assertEquals(1, clickEvent.getDetail());
  }

  @Test
  void shouldGetButton() {
    assertEquals(1, clickEvent.getButton());
  }

  @Test
  void shouldCheckIfCtrlKeyIsPressed() {
    assertTrue(clickEvent.isCtrlKey());
  }

  @Test
  void shouldCheckIfShiftKeyIsPressed() {
    assertFalse(clickEvent.isShiftKey());
  }

  @Test
  void shouldCheckIfAltKeyIsPressed() {
    assertTrue(clickEvent.isAltKey());
  }

  @Test
  void shouldCheckIfMetaKeyIsPressed() {
    assertFalse(clickEvent.isMetaKey());
  }
}
