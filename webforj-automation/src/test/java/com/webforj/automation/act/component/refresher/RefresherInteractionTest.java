package com.webforj.automation.act.component.refresher;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.component.refresher.Refresher;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class RefresherInteractionTest {

  @Test
  void shouldRefreshTheRefresher() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Refresher refresher = new Refresher();
      List<String> events = new ArrayList<>();
      refresher.onRefresh(event -> events.add("refresh"));
      frame.add(refresher);

      UserInteraction.perform(refresher, RefresherInteraction.refresh());

      assertEquals(List.of("refresh"), events);
    }
  }
}
