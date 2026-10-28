package com.webforj.automation.act;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.ObjectTableEnvironment;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.Component;
import com.webforj.component.alert.Alert;
import com.webforj.component.button.Button;
import com.webforj.component.element.ElementComposite;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class ComponentInteractionRegistryTest {

  @Test
  void shouldPerformTheVerbTheApplicationRegistered() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      CustomPicker picker = new CustomPicker("small", "large");
      frame.add(picker);
      picker.select("large");

      UserInteraction.close(picker);

      assertNull(picker.getSelected());
    }
  }

  @Test
  void shouldReturnTheRegistryTheAppHoldsInItsObjectTable() throws Exception {
    Environment environment = ObjectTableEnvironment.create();

    try (MockedStatic<Environment> current = mockStatic(Environment.class, CALLS_REAL_METHODS)) {
      current.when(Environment::getCurrent).thenReturn(environment);

      assertSame(ComponentInteractionRegistry.getCurrent(),
          ComponentInteractionRegistry.getCurrent());
    }
  }

  @Test
  void shouldReturnNewRegistryWhenTheAppHoldsNone() {
    assertNotSame(ComponentInteractionRegistry.getCurrent(),
        ComponentInteractionRegistry.getCurrent());
  }

  @Test
  void shouldFindTheInteractionsThatLoadWhenAnotherCannotLoad() {
    CustomPicker picker = new CustomPicker("small");

    ComponentInteractionRegistry registry = ComponentInteractionRegistry.getCurrent();

    assertInstanceOf(CustomPickerInteraction.class,
        registry.find(CloseInteraction.class, picker).orElseThrow());
  }

  @Test
  void shouldRejectTheVerbNoInteractionPerforms() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Button button = new Button("Save");
      frame.add(button);

      InteractionRejectedException failure =
          assertThrows(InteractionRejectedException.class, () -> UserInteraction.close(button));

      assertTrue(
          failure.getMessage().startsWith("The component Button cannot be closed by a user."));
      assertThrows(InteractionRejectedException.class, () -> UserInteraction.open(button));
    }
  }

  @Test
  void shouldAskTheApplicationBeforeTheShippedInteraction() {
    ComponentClick click = new ComponentClick();
    ComponentInteractionRegistry registry = new ComponentInteractionRegistry(List.of(click));

    assertSame(click, registry.find(ClickInteraction.class, new Button("Save")).orElseThrow());
  }

  @Test
  void shouldSkipTheInteractionThatDoesNotServeTheComponent() {
    Recorder refusing = new Recorder(Button.class, "refusing") {
      @Override
      public boolean isSupported(Component component) {
        return false;
      }
    };
    ComponentInteractionRegistry registry = new ComponentInteractionRegistry(List.of(refusing));

    assertTrue(registry.find(CloseInteraction.class, new Button("Save")).isEmpty());
  }

  @Test
  void shouldRejectTwoInteractionsOfTheSameVerbAndContract() {
    List<Recorder> twice =
        List.of(new Recorder(Alert.class, "one"), new Recorder(Alert.class, "two"));

    List<ComponentInteraction<?>> interactions = new ArrayList<>(twice);

    IllegalStateException failure = assertThrows(IllegalStateException.class,
        () -> new ComponentInteractionRegistry(interactions));

    assertTrue(failure.getMessage()
        .contains("both perform CloseInteraction for " + Alert.class.getName()));
  }

  @Test
  void shouldPreferTheInteractionOfTheComponentClassOverTheOneForAnyComponent() {
    PickerClick pickerClick = new PickerClick();
    ComponentInteractionRegistry registry =
        new ComponentInteractionRegistry(List.of(new ComponentClick(), pickerClick));
    CustomPicker picker = new CustomPicker("small");

    assertSame(pickerClick, registry.find(ClickInteraction.class, picker).orElseThrow());
  }

  @Test
  void shouldRejectTwoInteractionsOfComponentClassesThatServeTheSameComponent() {
    ComponentInteractionRegistry registry =
        new ComponentInteractionRegistry(List.of(new CompositeClick(), new PickerClick()));
    CustomPicker picker = new CustomPicker("small");

    IllegalStateException failure = assertThrows(IllegalStateException.class,
        () -> registry.find(ClickInteraction.class, picker));

    assertEquals("The interactions " + PickerClick.class.getName() + " and "
        + CompositeClick.class.getName() + " both perform ClickInteraction for CustomPicker. "
        + "Register one interaction that serves the component.", failure.getMessage());
  }

  @Test
  void shouldCloseThroughTheInteractionItWasGiven() {
    Recorder recorder = new Recorder(Component.class, "closed");
    Button button = new Button("Save");

    recorder.close(button);

    assertEquals(List.of("closed Button"), recorder.closed);
  }

  static class Recorder implements CloseInteraction<Component> {
    private final Class<?> contract;
    private final String name;
    private final List<String> closed = new ArrayList<>();

    Recorder(Class<?> contract, String name) {
      this.contract = contract;
      this.name = name;
    }

    @Override
    @SuppressWarnings("unchecked")
    public Class<Component> getContract() {
      return (Class<Component>) contract;
    }

    @Override
    public void close(Component component) {
      closed.add(name + " " + component.getClass().getSimpleName());
    }
  }

  static class ComponentClick implements ClickInteraction<Component> {

    @Override
    public Class<Component> getContract() {
      return Component.class;
    }

    @Override
    public void click(Component component, MouseOptions options) {
      // the click has no effect, the tests only look it up
    }
  }

  static class CompositeClick implements ClickInteraction<ElementComposite> {

    @Override
    public Class<ElementComposite> getContract() {
      return ElementComposite.class;
    }

    @Override
    public void click(ElementComposite composite, MouseOptions options) {
      // the click has no effect, the tests only look it up
    }
  }

  static class PickerClick implements ClickInteraction<CustomPicker> {

    @Override
    public Class<CustomPicker> getContract() {
      return CustomPicker.class;
    }

    @Override
    public void click(CustomPicker picker, MouseOptions options) {
      // the click has no effect, the tests only look it up
    }
  }
}
