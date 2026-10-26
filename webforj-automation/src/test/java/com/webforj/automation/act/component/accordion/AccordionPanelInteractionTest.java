package com.webforj.automation.act.component.accordion;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.accordion.Accordion;
import com.webforj.component.accordion.AccordionPanel;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;

class AccordionPanelInteractionTest {

  @Test
  void shouldReportTheToggleBeforeThePanelChanges() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      AccordionPanel panel = new AccordionPanel("Details");
      List<String> events = new ArrayList<>();
      panel.onToggle(event -> events.add("toggle to " + event.isOpened() + " " + panel.isOpened()));
      panel.onOpen(event -> events.add("open " + panel.isOpened()));
      panel.onClose(event -> events.add("close " + panel.isOpened()));
      frame.add(panel);

      UserInteraction.open(panel);
      UserInteraction.open(panel);
      UserInteraction.close(panel);
      UserInteraction.close(panel);

      assertEquals(
          List.of("toggle to true false", "open true", "toggle to false true", "close false"),
          events);
    }
  }

  @Test
  void shouldRejectTheDisabledPanel() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      AccordionPanel panel = new AccordionPanel("Details");
      frame.add(panel);
      panel.setEnabled(false);

      assertThrows(InteractionRejectedException.class, () -> UserInteraction.open(panel));
    }
  }

  @ParameterizedTest
  @MethodSource("createAccordionModes")
  void shouldCloseTheOtherPanelsOnlyWhenTheAccordionOpensOnePanel(boolean multiple,
      boolean firstOpened) throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      AccordionPanel first = new AccordionPanel("Details");
      AccordionPanel second = new AccordionPanel("Shipping");
      Accordion accordion = new Accordion(first, second).setMultiple(multiple);
      frame.add(accordion);
      first.open();
      List<String> events = new ArrayList<>();
      for (AccordionPanel panel : List.of(first, second)) {
        panel.onToggle(event -> events.add(panel.getLabel() + " toggle to " + event.isOpened()));
        panel.onOpen(event -> events.add(panel.getLabel() + " open"));
        panel.onClose(event -> events.add(panel.getLabel() + " close"));
      }

      UserInteraction.open(second);

      assertEquals(firstOpened, first.isOpened());
      assertEquals(List.of("Shipping toggle to true", "Shipping open"), events);
    }
  }

  private static Stream<Arguments> createAccordionModes() {
    return Stream.of(Arguments.of(false, false), Arguments.of(true, true));
  }
}
