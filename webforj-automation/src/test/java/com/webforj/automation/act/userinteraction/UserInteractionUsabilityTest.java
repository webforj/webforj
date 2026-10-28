package com.webforj.automation.act.userinteraction;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.component.Component;
import com.webforj.component.button.Button;
import com.webforj.component.dialog.Dialog;
import com.webforj.component.drawer.Drawer;
import com.webforj.component.element.Element;
import com.webforj.component.html.elements.Div;
import com.webforj.component.window.Frame;
import com.webforj.concern.HasComponents;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;

class UserInteractionUsabilityTest {
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
  void shouldRejectInteractionWithDetachedComponents() {
    Button button = new Button("Save");

    IllegalStateException failure =
        assertThrows(IllegalStateException.class, () -> UserInteraction.click(button));

    assertTrue(failure.getMessage().contains("not attached"));
  }

  @Test
  void shouldRejectInteractionWithHiddenComponents() {
    Button button = new Button("Save");
    frame.add(button);
    button.setVisible(false);

    IllegalStateException failure =
        assertThrows(IllegalStateException.class, () -> UserInteraction.click(button));

    assertTrue(failure.getMessage().contains("hidden"));
  }

  @Test
  void shouldRejectInteractionWithDisabledComponents() {
    Button button = new Button("Save");
    frame.add(button);
    button.setEnabled(false);

    IllegalStateException failure =
        assertThrows(IllegalStateException.class, () -> UserInteraction.click(button));

    assertTrue(failure.getMessage().contains("disabled"));
  }

  @Test
  void shouldNotPerformInteractionsOnUnreachableComponents() {
    Button hidden = new Button("Hidden");
    Button disabled = new Button("Disabled");
    frame.add(hidden, disabled);
    hidden.setVisible(false);
    disabled.setEnabled(false);
    List<Button> performed = new ArrayList<>();

    assertThrows(IllegalStateException.class,
        () -> UserInteraction.perform(hidden, performed::add));
    assertThrows(IllegalStateException.class,
        () -> UserInteraction.perform(disabled, performed::add));
    Button detached = new Button("Detached");
    assertThrows(IllegalStateException.class,
        () -> UserInteraction.perform(detached, performed::add));

    assertEquals(List.of(), performed);
  }

  @ParameterizedTest
  @MethodSource("createUnreachableHolders")
  void shouldRejectInteractionWithComponentsInsideUnreachableHolders(
      Supplier<HasComponents> holderFactory, String expected) {
    HasComponents holder = holderFactory.get();
    Button button = new Button("Save");
    List<String> clicked = new ArrayList<>();
    button.onClick(event -> clicked.add("Save"));
    holder.add(button);
    frame.add((Component) holder);

    IllegalStateException failure =
        assertThrows(IllegalStateException.class, () -> UserInteraction.click(button));

    assertEquals(expected, failure.getMessage().lines().findFirst().orElseThrow());
    assertEquals(List.of(), clicked);
  }

  @Test
  void shouldClickComponentsInsideTheOpenedDialog() {
    Dialog dialog = new Dialog();
    Button button = new Button("Save");
    List<String> clicked = new ArrayList<>();
    button.onClick(event -> clicked.add("Save"));
    dialog.add(button);
    frame.add(dialog);
    dialog.open();

    UserInteraction.click(button);

    assertEquals(List.of("Save"), clicked);
  }

  private static Stream<Arguments> createUnreachableHolders() {
    return Stream.of(
        Arguments.of((Supplier<HasComponents>) () -> new Div().setVisible(false),
            "The component Button is inside the hidden Div and a user cannot reach it."),
        Arguments.of((Supplier<HasComponents>) Dialog::new,
            "The component Button is inside the closed Dialog and a user cannot reach it."),
        Arguments.of((Supplier<HasComponents>) Drawer::new,
            "The component Button is inside the closed Drawer and a user cannot reach it."),
        Arguments.of((Supplier<HasComponents>) () -> new Element("div").setEnabled(false),
            "The component Button is inside the disabled Element and a user cannot reach it."));
  }
}
