package com.webforj.component.optiondialog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.optiondialog.Dialog.Alignment;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class DialogTest {

  private DialogMock component;

  @BeforeEach
  void setUp() {
    component = new DialogMock();
  }

  @Test
  void shouldSetAndGetAlignment() {
    component.setAlignment(Alignment.BOTTOM);
    assertEquals(Alignment.BOTTOM, component.getAlignment());
  }

  @Test
  void shouldToggleBlurredState() {
    component.setBlurred(true);
    assertTrue(component.isBlurred());
  }

  @Test
  void shouldHandleNullBreakpointGracefully() {
    component.setBreakpoint(null);
    assertEquals("", component.getBreakpoint());
  }

  @Test
  void shouldToggleFullscreenState() {
    component.setFullscreen(true);
    assertTrue(component.isFullscreen());
  }

  @Test
  void shouldToggleMoveableState() {
    component.setMoveable(false);
    assertFalse(component.isMoveable());
  }

  @Test
  void shouldSetAndGetHorizontalPosition() {
    component.setHorizontalPosition(200);
    assertEquals("200px", component.getHorizontalPosition());
  }

  @Test
  void shouldSetAndGetVerticalPosition() {
    component.setVerticalPosition(300);
    assertEquals("300px", component.getVerticalPosition());
  }

  @Test
  void shouldSetBothPositions() {
    component.setPosition(400, 500);
    assertEquals("400px", component.getHorizontalPosition());
    assertEquals("500px", component.getVerticalPosition());
  }

  @Test
  void shouldSetAndGetMaxWidth() {
    component.setMaxWidth(400);
    assertEquals("400px", component.getMaxWidth());
  }

  @Test
  void shouldSetAndGetMaxHeight() {
    component.setMaxHeight(600);
    assertEquals("600px", component.getMaxHeight());
  }

  @Test
  void shouldSetAndGetSnapThreshold() {
    component.setSnapThreshold(10);
    assertEquals(10, component.getSnapThreshold());
  }

  @Test
  void shouldToggleSnapToEdgeState() {
    component.setSnapToEdge(false);
    assertFalse(component.isSnapToEdge());
  }

  @Test
  void shouldAddAndRemoveAttributes() {
    component.setAttribute("key1", "value1");
    assertTrue(component.getAttributes().containsKey("key1"));

    component.removeAttribute("key1");
    assertFalse(component.getAttributes().containsKey("key1"));
  }

  @Test
  void shouldGetAttributesAsString() {
    component.setAttribute("key1", "value1");
    component.setAttribute("key2", "value2");

    assertEquals("key1=value1,key2=value2", component.getAttributesAsString());
  }
}
