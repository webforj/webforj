package com.webforj.automation.act.userinteraction;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.CustomValue;
import com.webforj.automation.act.UserInteraction;
import com.webforj.component.ComponentUtil;
import com.webforj.component.button.Button;
import com.webforj.component.element.Element;
import com.webforj.component.element.event.ElementClickEvent;
import com.webforj.component.event.MouseEnterEvent;
import com.webforj.component.html.elements.Div;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class UserInteractionFireEventTest {
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
  void shouldFireArbitraryElementEvents() {
    Div div = new Div();
    List<Object> payloads = new ArrayList<>();
    div.getElement().addEventListener("custom-event",
        event -> payloads.add(event.getEventMap().get("detail")));
    frame.add(div);

    UserInteraction.fireEvent(div, "custom-event", Map.of("detail", 42));

    assertEquals(List.of(42), payloads);
  }

  @Test
  void shouldFireEventsByNameOnTheBoundElementOfComposites() {
    CustomValue<String> value = new CustomValue<>("");
    List<Object> payloads = new ArrayList<>();
    ((Element) ComponentUtil.getBoundComponent(value)).addEventListener("x-change",
        event -> payloads.add(event.getEventMap().get("detail")));
    frame.add(value);

    UserInteraction.fireEvent(value, "x-change", Map.of("detail", "Ada"));

    assertEquals(List.of("Ada"), payloads);
  }

  @Test
  void shouldCarryEventDataAsTheEventExposesIt() {
    Button button = new Button("Save");
    List<Object> received = new ArrayList<>();
    button.onMouseEnter(event -> received.add(event.getMouseButton()));
    frame.add(button);

    UserInteraction.fireEvent(new MouseEnterEvent(button, Map.of("mouseButton", 2)));

    assertEquals(List.of(2), received);
  }

  @Test
  void shouldRejectEventsByNameOnComponentsThatListenByClass() {
    Button button = new Button("Save");
    frame.add(button);

    IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
        () -> UserInteraction.fireEvent(button, "click", Map.of()));

    assertEquals("Button does not listen to events by name. "
        + "Name the event class it exposes to its listeners.", failure.getMessage());
  }

  @Test
  void shouldRejectEventsOfDetachedComponents() {
    Div div = new Div();

    assertThrows(IllegalStateException.class,
        () -> UserInteraction.fireEvent(div, "custom-event", Map.of()));
    ElementClickEvent<Div> click = new ElementClickEvent<>(div, Map.of());
    assertThrows(IllegalStateException.class, () -> UserInteraction.fireEvent(click));
  }

  @Test
  void shouldRejectEventsWithoutEventName() {
    Div div = new Div();
    frame.add(div);

    NullPointerException failure = assertThrows(NullPointerException.class,
        () -> UserInteraction.fireEvent(div, null, Map.of()));

    assertEquals("The event name cannot be null", failure.getMessage());
  }
}
