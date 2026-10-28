package com.webforj.automation.act.component.alert;

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
import com.webforj.component.alert.Alert;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class AlertInteractionTest {

  @Test
  void shouldCloseTheAlertBeforeItReportsTheClose() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Alert alert = new Alert("Saved").setClosable(true);
      List<Boolean> opened = new ArrayList<>();
      alert.onClose(event -> opened.add(alert.isOpened()));
      frame.add(alert);
      alert.open();

      UserInteraction.close(alert);

      assertEquals(List.of(false), opened);
      assertFalse(alert.isOpened());
    }
  }

  @Test
  void shouldRejectClosingTheAlertWithoutCloseButton() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Alert alert = new Alert("Saved");
      frame.add(alert);

      InteractionRejectedException failure =
          assertThrows(InteractionRejectedException.class, () -> UserInteraction.close(alert));

      assertTrue(failure.getMessage()
          .startsWith("The component Alert has no close button and a user cannot close it."));
    }
  }
}
