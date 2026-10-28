package com.webforj.automation.act.component.terminal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.component.terminal.Terminal;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class TerminalInteractionTest {

  @Test
  void shouldTypeIntoTheTerminal() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Terminal terminal = new Terminal();
      List<String> events = new ArrayList<>();
      terminal.onData(event -> events.add(event.getValue()));
      frame.add(terminal);

      UserInteraction.type(terminal, "ls -la");

      assertEquals(List.of("ls -la"), events);
    }
  }
}
