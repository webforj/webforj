package com.webforj.automation.act.userinteraction;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.component.event.KeypressEvent;
import com.webforj.component.field.TextField;
import com.webforj.component.list.ComboBox;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class UserInteractionTypeTest {
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
  void shouldTypeIntoFieldsAndFireValueChange() {
    TextField field = new TextField();
    List<String> values = new ArrayList<>();
    field.addValueChangeListener(event -> values.add(event.getValue()));
    frame.add(field);

    UserInteraction.type(field, "hello");

    assertEquals(List.of("hello"), values);
    assertEquals("hello", field.getValue());
  }

  @Test
  void shouldRejectTypingIntoReadOnlyFields() {
    TextField field = new TextField();
    frame.add(field);
    field.setReadOnly(true);

    IllegalStateException failure =
        assertThrows(IllegalStateException.class, () -> UserInteraction.type(field, "hello"));

    assertTrue(failure.getMessage().contains("read only"));
  }

  @Test
  void shouldTypeIntoComboBoxes() {
    ComboBox comboBox = new ComboBox();
    comboBox.setAllowCustomValue(true);
    List<String> texts = new ArrayList<>();
    comboBox.onModify(event -> texts.add(event.getText()));
    frame.add(comboBox);

    UserInteraction.type(comboBox, "Ada");
    UserInteraction.keypress(comboBox, KeypressEvent.Key.ENTER);

    assertEquals(List.of("Ada"), texts);
    assertEquals("Ada", comboBox.getText());
  }

  @Test
  void shouldFireKeypressEvents() {
    TextField field = new TextField();
    List<KeypressEvent.Key> keys = new ArrayList<>();
    field.addKeypressListener(event -> keys.add(event.getKeyCode()));
    frame.add(field);

    UserInteraction.keypress(field, KeypressEvent.Key.ENTER);

    assertEquals(List.of(KeypressEvent.Key.ENTER), keys);
  }

  @Test
  void shouldMoveFocus() {
    TextField field = new TextField();
    List<String> events = new ArrayList<>();
    field.addFocusListener(event -> events.add("focus"));
    field.addBlurListener(event -> events.add("blur"));
    frame.add(field);

    UserInteraction.focus(field);

    assertEquals(List.of(), events);
  }
}
