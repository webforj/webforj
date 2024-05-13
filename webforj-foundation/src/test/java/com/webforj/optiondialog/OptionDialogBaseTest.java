package com.webforj.optiondialog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.Theme;
import com.webforj.optiondialog.OptionDialogBase.Alignment;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class OptionDialogBaseTest {

  private OptionDialogBaseMock dialog;

  @BeforeEach
  void setUp() {
    dialog = new OptionDialogBaseMock();
  }

  @Test
  void shouldSetAndGetAlignment() {
    dialog.setAlignment(Alignment.BOTTOM);
    assertEquals(Alignment.BOTTOM, dialog.getAlignment());
  }

  @Test
  void shouldToggleBlurredState() {
    dialog.setBlurred(true);
    assertTrue(dialog.isBlurred());
  }

  @Test
  void shouldHandleNullBreakpointGracefully() {
    dialog.setBreakpoint(null);
    assertEquals("", dialog.getBreakpoint());
  }

  @Test
  void shouldToggleFullscreenState() {
    dialog.setFullscreen(true);
    assertTrue(dialog.isFullscreen());
  }

  @Test
  void shouldToggleMoveableState() {
    dialog.setMoveable(false);
    assertFalse(dialog.isMoveable());
  }

  @Test
  void shouldSetAndGetHorizontalPosition() {
    dialog.setHorizontalPosition(200);
    assertEquals("200px", dialog.getHorizontalPosition());
  }

  @Test
  void shouldSetAndGetVerticalPosition() {
    dialog.setVerticalPosition(300);
    assertEquals("300px", dialog.getVerticalPosition());
  }

  @Test
  void shouldSetBothPositions() {
    dialog.setPosition(400, 500);
    assertEquals("400px", dialog.getHorizontalPosition());
    assertEquals("500px", dialog.getVerticalPosition());
  }

  @Test
  void shouldSetAndGetMaxWidth() {
    dialog.setMaxWidth(400);
    assertEquals("400px", dialog.getMaxWidth());
  }

  @Test
  void shouldSetAndGetMaxHeight() {
    dialog.setMaxHeight(600);
    assertEquals("600px", dialog.getMaxHeight());
  }

  @Test
  void shouldSetAndGetSnapThreshold() {
    dialog.setSnapThreshold(10);
    assertEquals(10, dialog.getSnapThreshold());
  }

  @Test
  void shouldToggleSnapToEdgeState() {
    dialog.setSnapToEdge(false);
    assertFalse(dialog.isSnapToEdge());
  }

  @Test
  void shouldSetAndGetTheme() {
    dialog.setTheme(Theme.PRIMARY);
    assertEquals(Theme.PRIMARY, dialog.getTheme());
  }

  @Test
  void shouldAddAndRemoveAttributes() {
    dialog.setAttribute("key1", "value1");
    assertTrue(dialog.getAttributes().containsKey("key1"));

    dialog.removeAttribute("key1");
    assertFalse(dialog.getAttributes().containsKey("key1"));
  }

  @Test
  void shouldGetAttributesAsString() {
    dialog.setAttribute("key1", "value1");
    dialog.setAttribute("key2", "value2");

    assertEquals("key1=value1,key2=value2", dialog.getAttributesAsString());
  }
}
