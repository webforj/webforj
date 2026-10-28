package com.webforj.automation.act.component.tabbedpane;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.html.elements.Paragraph;
import com.webforj.component.tabbedpane.Tab;
import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class TabbedPaneInteractionTest {

  @Test
  void shouldSelectTheTabBeforeItReportsTheSelection() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      TabbedPane pane = new TabbedPane();
      pane.addTab("First", new Paragraph("first"));
      Tab second = pane.addTab("Second", new Paragraph("second"));
      List<String> events = new ArrayList<>();
      pane.onSelect(event -> events.add(event.getTabIndex() + " " + pane.getSelectedIndex()));
      frame.add(pane);

      UserInteraction.select(pane, second);

      assertEquals(List.of("1 1"), events);
    }
  }

  @Test
  void shouldRejectTheTabTheUserCannotSelectOrClose() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      TabbedPane pane = new TabbedPane();
      Tab first = pane.addTab("First", new Paragraph("first"));
      Tab second = pane.addTab("Second", new Paragraph("second"));
      second.setEnabled(false);
      frame.add(pane);

      Consumer<TabbedPane> closeFirst = TabbedPaneInteraction.close(first);
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.perform(pane, closeFirst)).getMessage().contains("close button"));
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.select(pane, second)).getMessage().contains("disabled"));
      Tab foreign = new Tab("Foreign");
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.select(pane, foreign)).getMessage().contains("Foreign"));
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.select(pane, "First")).getMessage().contains("String"));
    }
  }

  @Test
  void shouldCloseTabs() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      TabbedPane tabbedPane = new TabbedPane();
      Tab first = tabbedPane.addTab("First", new Paragraph("first"));
      first.setClosable(true);
      List<Integer> received = new ArrayList<>();
      tabbedPane.onClose(event -> received.add(event.getTabIndex()));
      frame.add(tabbedPane);

      UserInteraction.perform(tabbedPane, TabbedPaneInteraction.close(first));

      assertEquals(List.of(0), received);
      assertEquals(List.of(first), tabbedPane.getTabs());
    }
  }

  @Test
  void shouldRemoveTheClosedTabWhenThePaneRemovesTabsItself() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      TabbedPane tabbedPane = new TabbedPane().setRemoval(TabbedPane.Removal.AUTO);
      tabbedPane.addTab("First", new Paragraph("first"));
      Tab second = tabbedPane.addTab("Second", new Paragraph("second"));
      second.setClosable(true);
      List<Integer> received = new ArrayList<>();
      tabbedPane.onClose(event -> received.add(event.getTabIndex()));
      frame.add(tabbedPane);

      UserInteraction.perform(tabbedPane, TabbedPaneInteraction.close(second));

      assertEquals(List.of(1), received);
      assertEquals(List.of("First"), tabbedPane.getTabs().stream().map(Tab::getText).toList());
    }
  }

  @Test
  void shouldDeselectTheSelectedTabBeforeSelectingAnotherOne() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      TabbedPane pane = new TabbedPane();
      pane.addTab("First", new Paragraph("first"));
      pane.addTab("Second", new Paragraph("second"));
      List<String> events = new ArrayList<>();
      pane.onDeselect(event -> events.add("deselect " + event.getTab().getText()));
      pane.onSelect(event -> events.add("select " + event.getTab().getText()));
      frame.add(pane);
      Tab second = pane.getTabs().get(1);

      UserInteraction.select(pane, second);
      UserInteraction.select(pane, second);

      assertEquals(List.of("deselect First", "select Second", "select Second"), events);
      assertEquals(second, pane.getSelected());
    }
  }
}
