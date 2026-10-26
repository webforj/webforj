package com.webforj.automation.act.component.applayout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.component.layout.applayout.AppLayout;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class AppLayoutInteractionTest {

  @Test
  void shouldOpenAndCloseTheDrawerOfTheLayout() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      AppLayout layout = new AppLayout();
      List<String> events = new ArrayList<>();
      layout.onDrawerOpen(event -> events.add("open " + layout.isDrawerOpened()));
      layout.onDrawerClose(event -> events.add("close " + layout.isDrawerOpened()));
      frame.add(layout);

      UserInteraction.open(layout);
      assertTrue(layout.isDrawerOpened());
      UserInteraction.close(layout);

      assertFalse(layout.isDrawerOpened());
      assertEquals(List.of(), events);
    }
  }
}
