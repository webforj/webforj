package com.webforj.automation.act.component.dialog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.dialog.Dialog;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class DialogInteractionTest {

  @Test
  void shouldCloseTheOpenDialogWithoutReportingTheClose() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Dialog dialog = new Dialog();
      List<Boolean> opened = new ArrayList<>();
      dialog.onClose(event -> opened.add(dialog.isOpened()));
      frame.add(dialog);
      dialog.open();

      UserInteraction.close(dialog);

      assertFalse(dialog.isOpened());
      assertEquals(List.of(), opened);
    }
  }

  @Test
  void shouldLeaveTheClosedDialogClosed() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Dialog dialog = new Dialog();
      List<Boolean> opened = new ArrayList<>();
      dialog.onClose(event -> opened.add(dialog.isOpened()));
      frame.add(dialog);

      UserInteraction.close(dialog);

      assertFalse(dialog.isOpened());
      assertEquals(List.of(), opened);
    }
  }

  @Test
  void shouldRejectClosingTheDialogTheUserCannotClose() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Dialog locked = new Dialog().setCancelOnEscKey(false).setCancelOnOutsideClick(false);
      frame.add(locked);
      locked.open();

      assertTrue(
          assertThrows(InteractionRejectedException.class, () -> UserInteraction.close(locked))
              .getMessage().contains("Escape key"));
      assertTrue(locked.isOpened());
    }
  }
}
