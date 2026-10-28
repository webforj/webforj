package com.webforj.automation.act.userinteraction;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.component.optioninput.CheckBox;
import com.webforj.component.optioninput.RadioButton;
import com.webforj.component.optioninput.RadioButtonGroup;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class UserInteractionCheckTest {
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
  void shouldCheckAndUncheckOptionInputs() {
    CheckBox checkBox = new CheckBox("Accept");
    List<Boolean> toggles = new ArrayList<>();
    checkBox.addToggleListener(event -> toggles.add(event.isToggled()));
    frame.add(checkBox);

    UserInteraction.check(checkBox);

    assertTrue(checkBox.isChecked());

    UserInteraction.uncheck(checkBox);

    assertFalse(checkBox.isChecked());
    assertEquals(List.of(true, false), toggles);
  }

  @Test
  void shouldToggleOptionInputsOnClick() {
    CheckBox checkBox = new CheckBox("Accept");
    frame.add(checkBox);

    UserInteraction.click(checkBox);
    assertTrue(checkBox.isChecked());

    UserInteraction.click(checkBox);
    assertFalse(checkBox.isChecked());
  }

  @Test
  void shouldReportTheMemberEventsBeforeTheGroupChange() {
    RadioButton small = new RadioButton("Small");
    RadioButton large = new RadioButton("Large");
    RadioButtonGroup group = new RadioButtonGroup("size", small, large);
    List<String> order = new ArrayList<>();
    large.onCheck(event -> order.add("check"));
    large.onToggle(event -> order.add("toggle"));
    group.onChange(event -> order.add(
        "change from " + (event.getUnchecked() == null ? "none" : event.getUnchecked().getText())));
    frame.add(small, large, group);

    UserInteraction.check(large);
    UserInteraction.check(small);

    assertEquals(List.of("check", "toggle", "change from none"), order.subList(0, 3));
    assertEquals("change from Large", order.get(order.size() - 1));
  }
}
