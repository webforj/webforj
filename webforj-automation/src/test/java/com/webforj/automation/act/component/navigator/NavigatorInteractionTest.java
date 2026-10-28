package com.webforj.automation.act.component.navigator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.component.navigator.Navigator;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class NavigatorInteractionTest {

  @Test
  void shouldMoveTheNavigatorWithinItsPages() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Navigator navigator = new Navigator(25, 10);
      List<String> events = new ArrayList<>();
      navigator.onMoveFirst(event -> events.add("first " + event.getCurrent()));
      navigator.onMoveLast(event -> events.add("last " + event.getCurrent()));
      navigator.onMoveNext(event -> events.add("next " + event.getCurrent()));
      navigator.onMovePrevious(event -> events.add("previous " + event.getCurrent()));
      frame.add(navigator);

      UserInteraction.perform(navigator, NavigatorInteraction.movePrevious());
      UserInteraction.perform(navigator, NavigatorInteraction.moveNext());
      UserInteraction.perform(navigator, NavigatorInteraction.moveLast());
      UserInteraction.perform(navigator, NavigatorInteraction.moveFirst());

      assertEquals(List.of("previous 1", "next 2", "last 3", "first 1"), events);
    }
  }
}
