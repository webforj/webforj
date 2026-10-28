package com.webforj.automation.act.component.infinitescroll;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.infinitescroll.InfiniteScroll;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class InfiniteScrollInteractionTest {

  @Test
  void shouldAskForMoreUntilTheScrollIsCompleted() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      InfiniteScroll scroll = new InfiniteScroll();
      List<String> events = new ArrayList<>();
      scroll.onScroll(event -> events.add("more"));
      frame.add(scroll);

      UserInteraction.perform(scroll, InfiniteScrollInteraction.scroll());
      scroll.setCompleted(true);

      Consumer<InfiniteScroll> scrollMore = InfiniteScrollInteraction.scroll();
      assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.perform(scroll, scrollMore));
      assertEquals(List.of("more"), events);
    }
  }
}
