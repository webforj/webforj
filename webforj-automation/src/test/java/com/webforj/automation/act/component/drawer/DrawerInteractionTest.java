package com.webforj.automation.act.component.drawer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.component.drawer.Drawer;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class DrawerInteractionTest {

  @Test
  void shouldCloseTheOpenDrawerAndLeaveTheClosedOneClosed() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Drawer drawer = new Drawer();
      List<Boolean> opened = new ArrayList<>();
      drawer.onClose(event -> opened.add(drawer.isOpened()));
      frame.add(drawer);

      UserInteraction.close(drawer);
      drawer.open();
      UserInteraction.close(drawer);

      assertFalse(drawer.isOpened());
      assertEquals(List.of(), opened);
    }
  }
}
