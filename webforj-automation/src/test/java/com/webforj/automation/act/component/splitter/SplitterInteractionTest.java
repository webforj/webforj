package com.webforj.automation.act.component.splitter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.layout.splitter.Splitter;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class SplitterInteractionTest {

  @Test
  void shouldMoveTheSplitterBeforeItReportsThePosition() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Splitter splitter = new Splitter();
      List<String> events = new ArrayList<>();
      splitter.onResize(event -> events.add(event.getPositionRelative() + " " + event.isAdjusting()
          + " " + splitter.getPositionRelative()));
      frame.add(splitter);

      UserInteraction.perform(splitter, SplitterInteraction.resize(30));

      assertEquals(List.of("30.0 false 30.0"), events);
      Consumer<Splitter> resizeOutOfRange = SplitterInteraction.resize(120);
      assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.perform(splitter, resizeOutOfRange));
    }
  }
}
