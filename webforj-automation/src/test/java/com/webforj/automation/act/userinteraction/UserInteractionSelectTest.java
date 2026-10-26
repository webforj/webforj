package com.webforj.automation.act.userinteraction;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.CustomPicker;
import com.webforj.automation.act.UserInteraction;
import com.webforj.component.list.ChoiceBox;
import com.webforj.component.list.ListBox;
import com.webforj.component.list.ListItem;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class UserInteractionSelectTest {
  private MockedStatic<Environment> current;
  private MockedStatic<App> app;
  private Frame frame;

  @BeforeEach
  void initFrame() throws Exception {
    Environment environment = FrameEnvironment.create(null);
    current = mockStatic(Environment.class);
    app = mockStatic(App.class);
    current.when(Environment::getCurrent).thenReturn(environment);
    frame = new Frame("Orders");
    app.when(App::getFrames).thenReturn(List.of(frame));
  }

  @AfterEach
  void closeMocks() {
    app.close();
    current.close();
  }

  @Test
  void shouldSelectListItems() {
    ChoiceBox choiceBox = new ChoiceBox();
    choiceBox.add("one", "One");
    choiceBox.add("two", "Two");
    List<Object> selections = new ArrayList<>();
    choiceBox.addSelectListener(event -> selections.add(event.getSelectedItem().getKey()));
    frame.add(choiceBox);

    UserInteraction.selectIndex(choiceBox, 1);

    assertEquals(List.of("two"), selections);
    assertEquals(1, choiceBox.getSelectedIndex());
  }

  @Test
  void shouldSelectListItemsByItemAndByKey() {
    ListBox listBox = new ListBox();
    listBox.add("one", "One");
    listBox.add("two", "Two");
    List<Object> selections = new ArrayList<>();
    listBox.addSelectListener(event -> selections.add(event.getSelectedItem().getKey()));
    frame.add(listBox);

    ListItem two = listBox.getByKey("two");
    UserInteraction.select(listBox, two);
    UserInteraction.selectKey(listBox, "one");

    assertEquals(List.of("two", "one"), selections);
    assertEquals(0, listBox.getSelectedIndex());
  }

  @Test
  void shouldRejectSelectingItemsTheComponentDoesNotContain() {
    ChoiceBox choiceBox = new ChoiceBox();
    choiceBox.add("one", "One");
    frame.add(choiceBox);

    assertThrows(IndexOutOfBoundsException.class, () -> UserInteraction.selectIndex(choiceBox, 3));
    assertThrows(IllegalArgumentException.class, () -> UserInteraction.selectKey(choiceBox, "two"));
  }

  @Test
  void shouldRejectSelectingWhatTheComponentDoesNotHold() {
    ChoiceBox choiceBox = new ChoiceBox();
    choiceBox.add("one", "One");
    CustomPicker picker = new CustomPicker("small");
    frame.add(choiceBox, picker);

    assertThrows(IllegalStateException.class, () -> UserInteraction.select(choiceBox, "one"));
    assertThrows(IllegalStateException.class, () -> UserInteraction.select(picker, "large"));
    assertThrows(IllegalStateException.class, () -> UserInteraction.selectKey(picker, "large"));
    assertThrows(IllegalStateException.class, () -> UserInteraction.selectIndex(picker, 4));
  }
}
