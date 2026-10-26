package com.webforj.automation.act.component.googlecharts;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.component.googlecharts.GoogleChart;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class GoogleChartInteractionTest {

  @Test
  void shouldHoldTheSelectionOfTheChartBeforeItReportsIt() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      GoogleChart chart = new GoogleChart();
      List<Object> selections = new ArrayList<>();
      chart.onSelect(event -> selections.add(event.getSelection() + " " + chart.getSelection()));
      frame.add(chart);

      UserInteraction.select(chart, List.of("row 1", "row 2"));
      UserInteraction.select(chart, "row 3");

      assertEquals(List.of("[row 1, row 2] [row 1, row 2]", "[row 3] [row 3]"), selections);
    }
  }
}
