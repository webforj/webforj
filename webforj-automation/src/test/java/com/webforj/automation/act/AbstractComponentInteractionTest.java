package com.webforj.automation.act;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.button.Button;
import com.webforj.component.button.event.ButtonClickEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;

class AbstractComponentInteractionTest {

  @Test
  void shouldDeliverTheFiredEventOnceToTheListenersOfItsComponent() {
    Button button = new Button("Save");
    List<ButtonClickEvent> received = new ArrayList<>();
    button.onClick(received::add);
    ButtonClickEvent event = new ButtonClickEvent(button, Map.of());

    AbstractComponentInteraction.fire(event);

    assertEquals(List.of(event), received);
  }
}
