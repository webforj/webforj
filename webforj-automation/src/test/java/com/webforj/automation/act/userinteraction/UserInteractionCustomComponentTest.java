package com.webforj.automation.act.userinteraction;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.CustomPicker;
import com.webforj.automation.act.CustomPickerInteraction;
import com.webforj.automation.act.CustomValue;
import com.webforj.automation.act.UserInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.button.Button;
import com.webforj.component.event.KeypressEvent;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class UserInteractionCustomComponentTest {
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
  void shouldRejectUnsupportedVerbs() {
    Button button = new Button("Save");
    frame.add(button);

    assertThrows(IllegalStateException.class, () -> UserInteraction.type(button, "text"));
    assertThrows(IllegalStateException.class, () -> UserInteraction.check(button));
    assertThrows(IllegalStateException.class, () -> UserInteraction.selectIndex(button, 0));
  }

  @Test
  void shouldRejectCustomComponentsWithoutTheContract() {
    CustomValue<String> input = new CustomValue<>("");
    input.setReadOnly(true);
    frame.add(input);

    assertThrows(IllegalStateException.class, () -> UserInteraction.type(input, "Ada"));
    assertThrows(IllegalStateException.class, () -> UserInteraction.check(input));
    assertThrows(IllegalStateException.class, () -> UserInteraction.select(input, "x"));
    assertThrows(IllegalStateException.class,
        () -> UserInteraction.keypress(input, KeypressEvent.Key.ENTER));
  }

  @Test
  void shouldPerformTheInteractionsOfCustomComponents() {
    CustomPicker picker = new CustomPicker("small", "medium");
    frame.add(picker);

    UserInteraction.perform(picker, CustomPickerInteraction.pick("medium"));

    assertEquals("medium", picker.getSelected());

    UserInteraction.perform(picker, CustomPicker::deselect);

    assertEquals(-1, picker.getSelectedIndex());
  }

  @Test
  void shouldRejectInteractionsTheComponentRefuses() {
    CustomPicker picker = new CustomPicker("small", "medium");
    frame.add(picker);

    Consumer<CustomPicker> pickLarge = CustomPickerInteraction.pick("large");

    InteractionRejectedException failure = assertThrows(InteractionRejectedException.class,
        () -> UserInteraction.perform(picker, pickLarge));

    assertTrue(failure.getMessage().contains("The component CustomPicker does not offer large."));
  }

  @Test
  void shouldTreatCustomComponentsLikeCoreOnesThroughTheirContracts() {
    CustomValue<String> input = new CustomValue<>("");
    CustomValue<Boolean> toggle = new CustomValue<>(false);
    CustomPicker picker = new CustomPicker("small", "medium", "large");
    List<Object> values = new ArrayList<>();
    input.onValueChange(event -> values.add(event.getValue()));
    toggle.onValueChange(event -> values.add(event.getValue()));
    frame.add(input, toggle, picker);

    UserInteraction.type(input, "Ada");
    UserInteraction.check(toggle);
    UserInteraction.click(toggle);
    UserInteraction.select(picker, "medium");
    UserInteraction.selectIndex(picker, 2);
    UserInteraction.selectKey(picker, "small");

    assertEquals(List.of("Ada", true, false), values);
    assertEquals("small", picker.getSelected());
  }
}
