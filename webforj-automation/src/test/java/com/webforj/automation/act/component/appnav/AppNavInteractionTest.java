package com.webforj.automation.act.component.appnav;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.component.layout.appnav.AppNav;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class AppNavInteractionTest {

  @Test
  void shouldReportTheSearchTerm() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      AppNav nav = new AppNav();
      List<String> terms = new ArrayList<>();
      nav.onSearch(event -> terms.add(event.getTerm() + " " + nav.getSearch().getTerm()));
      frame.add(nav);

      UserInteraction.perform(nav, AppNavInteraction.search("inv"));

      assertEquals(List.of("inv inv"), terms);
    }
  }
}
