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
  void shouldSetAndGetAlignmentCorrectly() {
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
  void shouldSetAndGetHorizontalPositionCorrectly() {
    dialog.setHorizontalPosition(200);
    assertEquals("200px", dialog.getHorizontalPosition());
  }

  @Test
  void shouldSetAndGetVerticalPositionCorrectly() {
    dialog.setVerticalPosition(300);
    assertEquals("300px", dialog.getVerticalPosition());
  }

  @Test
  void shouldSetAndGetMaxWidthCorrectly() {
    dialog.setMaxWidth(400);
    assertEquals("400px", dialog.getMaxWidth());
  }

  @Test
  void shouldSetAndGetMaxHeightCorrectly() {
    dialog.setMaxHeight(600);
    assertEquals("600px", dialog.getMaxHeight());
  }

  @Test
  void shouldSetAndGetSnapThresholdCorrectly() {
    dialog.setSnapThreshold(10);
    assertEquals(10, dialog.getSnapThreshold());
  }

  @Test
  void shouldToggleSnapToEdgeState() {
    dialog.setSnapToEdge(false);
    assertFalse(dialog.isSnapToEdge());
  }

  @Test
  void shouldSetAndGetThemeCorrectly() {
    dialog.setTheme(Theme.PRIMARY);
    assertEquals(Theme.PRIMARY, dialog.getTheme());
  }

  @Test
  void shouldAddAndRemoveAttributesCorrectly() {
    dialog.setAttribute("key1", "value1");
    assertTrue(dialog.getAttributes().containsKey("key1"));

    dialog.removeAttribute("key1");
    assertFalse(dialog.getAttributes().containsKey("key1"));
  }
}
