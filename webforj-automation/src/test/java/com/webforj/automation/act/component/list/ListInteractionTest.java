package com.webforj.automation.act.component.list;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.list.ChoiceBox;
import com.webforj.component.list.ListBox;
import com.webforj.component.list.ListItem;
import com.webforj.component.list.MultipleSelectableList.SelectionMode;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class ListInteractionTest {

  @Test
  void shouldDeselectListItemsAndReportTheSelectionLeft() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      ListBox listBox = new ListBox();
      listBox.setSelectionMode(SelectionMode.MULTIPLE);
      listBox.add("one", "One");
      listBox.add("two", "Two");
      List<List<Integer>> selections = new ArrayList<>();
      listBox.addSelectListener(event -> selections.add(event.getSelectedIndices()));
      frame.add(listBox);
      listBox.selectKey("one", "two");

      UserInteraction.deselect(listBox, listBox.getByKey("one"));

      assertEquals(List.of(List.of(1)), selections);
      assertEquals(List.of(1), listBox.getSelectedIndices());
    }
  }

  @Test
  void shouldOpenCloseAndClickTheDropdown() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      List<String> events = new ArrayList<>();
      ChoiceBox choiceBox = new ChoiceBox();
      choiceBox.onOpen(event -> events.add("open " + event.getSelectedIndices()));
      choiceBox.onClose(event -> events.add("close " + event.getSelectedIndex()));
      choiceBox.onClick(event -> events.add("click " + event.getSelectedItem().getText()));
      choiceBox.onSelect(event -> events.add("select " + event.getSelectedIndex()));
      choiceBox.add("apple", "Apple");
      choiceBox.add("pear", "Pear");
      frame.add(choiceBox);
      ListItem pear = choiceBox.getByKey("pear");

      UserInteraction.open(choiceBox);
      UserInteraction.open(choiceBox);
      UserInteraction.click(choiceBox, pear);
      UserInteraction.close(choiceBox);

      assertEquals(List.of("open []", "click Pear", "close 1", "select 1"), events);
      assertEquals(pear, choiceBox.getSelected());
      assertFalse(choiceBox.isOpened());
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.click(choiceBox, pear)).getMessage().contains("is not open"));
      ListItem foreign = new ListBox().add("plum", "Plum");
      UserInteraction.open(choiceBox);
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.click(choiceBox, foreign)).getMessage().contains("Plum"));
    }
  }

  @Test
  void shouldSelectAndDeselectInTheListThatSelectsSeveralItems() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      ListBox listBox = new ListBox();
      listBox.add("one", "One");
      listBox.add("two", "Two");
      listBox.setSelectionMode(ListBox.SelectionMode.MULTIPLE);
      ChoiceBox choiceBox = new ChoiceBox();
      choiceBox.add("only", "Only");
      List<List<Integer>> selections = new ArrayList<>();
      listBox.onSelect(event -> selections.add(event.getSelectedIndices()));
      frame.add(listBox, choiceBox);

      UserInteraction.selectIndex(listBox, 1);
      UserInteraction.selectKey(listBox, "one");
      ListItem two = listBox.getByKey("two");
      UserInteraction.deselect(listBox, two);
      UserInteraction.deselect(listBox, two);

      assertEquals(List.of(List.of(1), List.of(1, 0), List.of(0)), selections);
      assertEquals(List.of(listBox.getByKey("one")), listBox.getSelectedItems());
      ListItem only = choiceBox.getByKey("only");
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.deselect(choiceBox, only)).getMessage()
          .contains("does not deselect items"));
    }
  }

  @Test
  void shouldSelectInTheListThatSelectsOneItem() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      ChoiceBox choiceBox = new ChoiceBox();
      choiceBox.add("apple", "Apple");
      ListItem pear = choiceBox.add("pear", "Pear");
      List<List<Integer>> selections = new ArrayList<>();
      choiceBox.onSelect(event -> selections.add(event.getSelectedIndices()));
      frame.add(choiceBox);

      UserInteraction.select(choiceBox, pear);
      UserInteraction.selectKey(choiceBox, "apple");

      assertEquals(List.of(List.of(1), List.of(0)), selections);
      assertEquals("apple", choiceBox.getSelectedKey());
    }
  }

  @Test
  void shouldRejectOpeningClosingAndClickingTheListWithoutDropdown() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      ListBox listBox = new ListBox();
      ListItem one = listBox.add("one", "One");
      frame.add(listBox);

      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.click(listBox, one)).getMessage()
          .contains("has no dropdown items to click"));
      assertTrue(
          assertThrows(InteractionRejectedException.class, () -> UserInteraction.open(listBox))
              .getMessage().contains("has no dropdown to open"));
      assertTrue(
          assertThrows(InteractionRejectedException.class, () -> UserInteraction.close(listBox))
              .getMessage().contains("has no dropdown to close"));
    }
  }
}
