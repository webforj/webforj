package org.dwcj.component.element.annotation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Map;
import org.dwcj.component.Component;
import org.dwcj.component.element.ElementComposite;
import org.dwcj.component.element.event.DebouncePhase;
import org.dwcj.component.element.event.ElementEventOptions;
import org.dwcj.component.event.ComponentEvent;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ElementAnnotationProcessorTest {

  @Test
  @DisplayName("should process @NodeName annotation")
  void shouldProcessNodeNameAnnotation() {
    @NodeName("div")
    class Div extends ElementComposite {
    }

    String nodeName = ElementAnnotationProcessor.processNodeName(Div.class);
    assertEquals("div", nodeName);
  }

  @Test
  @DisplayName("should throw exception when missing @NodeName annotation")
  void shouldThrowExceptionWhenMissingNodeNameAnnotation() {
    class Div extends ElementComposite {
    }

    assertThrows(DwcjRuntimeException.class, () -> {
      ElementAnnotationProcessor.processNodeName(Div.class);
    });
  }

  @Test
  @DisplayName("should process @NodeName annotation")
  void shouldProcessEventNameAnnotation() {
    @EventName("click")
    class Div extends ComponentEvent<Component> {
      public Div(Component component, Map<String, Object> eventMap) {
        super(component, eventMap);
      }
    }

    String eventName = ElementAnnotationProcessor.processEventName(Div.class);
    assertEquals("click", eventName);
  }

  @Test
  @DisplayName("should throw exception when missing @EventName annotation")
  void shouldThrowExceptionWhenMissingEventNameAnnotation() {
    class Div extends ComponentEvent<Component> {
      public Div(Component component, Map<String, Object> eventMap) {
        super(component, eventMap);
      }
    }

    assertThrows(DwcjRuntimeException.class, () -> {
      ElementAnnotationProcessor.processEventName(Div.class);
    });
  }

  @Test
  @DisplayName("should throw exception when @EventName annotation has empty value")
  void shouldThrowExceptionWhenEventNameAnnotationHasEmptyValue() {
    @EventName("")
    class Div extends ComponentEvent<Component> {
      public Div(Component component, Map<String, Object> eventMap) {
        super(component, eventMap);
      }
    }

    assertThrows(DwcjRuntimeException.class, () -> {
      ElementAnnotationProcessor.processEventName(Div.class);
    });
  }

  @Test
  @DisplayName("should process @EventOptions annotation")
  void shouldProcessEventOptionsAnnotation() {
    @EventOptions(code = "123", filter = "sampleFilter",
        map = {@EventOptions.EventMapItem(key = "key1", exp = "expr1"),
            @EventOptions.EventMapItem(key = "key2", exp = "expr2")},
        debounce = @EventOptions.DebounceSettings(value = 100, phase = DebouncePhase.BOTH))
    class Listener {
    }

    ElementEventOptions eventOptions =
        ElementAnnotationProcessor.processEventOptions(Listener.class);

    assertEquals("123", eventOptions.getCode());
    assertEquals("sampleFilter", eventOptions.getFilter());
    assertEquals("expr1", eventOptions.getItem("key1"));
    assertEquals("expr2", eventOptions.getItem("key2"));
    assertEquals(100, eventOptions.getDebounceTimeout());
    assertEquals(DebouncePhase.BOTH, eventOptions.getDebouncePhase());
  }

  @Test
  @DisplayName("should return empty event options when missing @EventOptions annotation")
  void shouldReturnEmptyEventOptionsWhenMissingEventOptionsAnnotation() {
    class Listener {
    }

    ElementEventOptions eventOptions =
        ElementAnnotationProcessor.processEventOptions(Listener.class);

    assertEquals("", eventOptions.getCode());
    assertEquals("", eventOptions.getFilter());
    assertTrue(eventOptions.getItems().isEmpty());
  }
}
