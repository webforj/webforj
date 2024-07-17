package com.webforj.component.terminal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.PendingResult;
import com.webforj.component.element.Element;
import com.webforj.component.element.PropertyDescriptorTester;
import com.webforj.component.terminal.event.TerminalDataEvent;
import com.webforj.component.terminal.event.TerminalKeyEvent;
import com.webforj.dispatcher.EventListener;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class TerminalTest {

  Terminal component;

  @BeforeEach
  void setUp() {
    component = new Terminal();
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(Terminal.class, component);
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }
  }

  @Nested
  @DisplayName("Methods API")
  class MethodsApi {

    @Test
    void shouldFocus() {
      Terminal mock = spy(Terminal.class);
      Element element = mock(Element.class);
      when(mock.getOriginalElement()).thenReturn(element);

      mock.focus();
      verify(element).callJsFunctionAsync(Terminal.JS_METHOD_SET_FOCUS);
    }

    @Test
    void shouldBlur() {
      Terminal mock = spy(Terminal.class);
      Element element = mock(Element.class);
      when(mock.getOriginalElement()).thenReturn(element);

      mock.blur();
      verify(element).callJsFunctionAsync(Terminal.JS_METHOD_REMOVE_FOCUS);
    }

    @Test
    void shouldClear() {
      Terminal mock = spy(Terminal.class);
      Element element = mock(Element.class);
      when(mock.getOriginalElement()).thenReturn(element);

      mock.clear();
      verify(element).callJsFunctionAsync(Terminal.JS_METHOD_CLEAR);
    }

    @Test
    void shouldReset() {
      Terminal mock = spy(Terminal.class);
      Element element = mock(Element.class);
      when(mock.getOriginalElement()).thenReturn(element);

      mock.reset();
      verify(element).callJsFunctionAsync(Terminal.JS_METHOD_RESET);
    }

    @Test
    void shouldFit() {
      Terminal mock = spy(Terminal.class);
      Element element = mock(Element.class);
      when(mock.getOriginalElement()).thenReturn(element);

      mock.fit();
      verify(element).callJsFunctionAsync(Terminal.JS_METHOD_FIT);
    }

    @Test
    void shouldSelectAll() {
      Terminal mock = spy(Terminal.class);
      Element element = mock(Element.class);
      when(mock.getOriginalElement()).thenReturn(element);

      mock.selectAll();
      verify(element).callJsFunctionAsync(Terminal.JS_METHOD_SELECT_ALL);
    }

    @Test
    void shouldSelect() {
      Terminal mock = spy(Terminal.class);
      Element element = mock(Element.class);
      when(mock.getOriginalElement()).thenReturn(element);

      mock.select(1, 2, 3);
      verify(element).callJsFunctionAsync(Terminal.JS_METHOD_SELECT, 1, 2, 3);
    }

    @Test
    void shouldSelectLines() {
      Terminal mock = spy(Terminal.class);
      Element element = mock(Element.class);
      when(mock.getOriginalElement()).thenReturn(element);

      mock.selectLines(1, 2);
      verify(element).callJsFunctionAsync(Terminal.JS_METHOD_SELECT_LINES, 1, 2);
    }

    @Test
    void shouldClearSelection() {
      Terminal mock = spy(Terminal.class);
      Element element = mock(Element.class);
      when(mock.getOriginalElement()).thenReturn(element);

      mock.clearSelection();
      verify(element).callJsFunctionAsync(Terminal.JS_METHOD_CLEAR_SELECTION);
    }

    @Test
    void shouldGetSelection() {
      Terminal mock = spy(Terminal.class);
      Element element = mock(Element.class);
      when(mock.getOriginalElement()).thenReturn(element);
      when(element.callJsFunctionAsync(Terminal.JS_METHOD_GET_SELECTION))
          .thenReturn(PendingResult.completedWith("Hello"));

      PendingResult<String> selected = mock.getSelectedText();
      selected.thenAccept(text -> assertEquals("Hello", text));
    }

    @Test
    void shouldScrollLines() {
      Terminal mock = spy(Terminal.class);
      Element element = mock(Element.class);
      when(mock.getOriginalElement()).thenReturn(element);

      mock.scrollLines(1);
      verify(element).callJsFunctionAsync(Terminal.JS_METHOD_SCROLL_LINES, 1);
    }

    @Test
    void shouldScrollToLine() {
      Terminal mock = spy(Terminal.class);
      Element element = mock(Element.class);
      when(mock.getOriginalElement()).thenReturn(element);

      mock.scrollToLine(1);
      verify(element).callJsFunctionAsync(Terminal.JS_METHOD_SCROLL_TO_LINE, 1);
    }

    @Test
    void shouldScrollToTop() {
      Terminal mock = spy(Terminal.class);
      Element element = mock(Element.class);
      when(mock.getOriginalElement()).thenReturn(element);

      mock.scrollToTop();
      verify(element).callJsFunctionAsync(Terminal.JS_METHOD_SCROLL_TO_TOP);
    }

    @Test
    void shouldScrollToBottom() {
      Terminal mock = spy(Terminal.class);
      Element element = mock(Element.class);
      when(mock.getOriginalElement()).thenReturn(element);

      mock.scrollToBottom();
      verify(element).callJsFunctionAsync(Terminal.JS_METHOD_SCROLL_TO_BOTTOM);
    }
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    void shouldAddDataListener() {
      component.onData(event -> {
      });

      List<EventListener<TerminalDataEvent>> listeners =
          component.getEventListeners(TerminalDataEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<TerminalDataEvent>);
    }

    @Test
    void shouldAddKeyListener() {
      component.onKey(event -> {
      });

      List<EventListener<TerminalKeyEvent>> listeners =
          component.getEventListeners(TerminalKeyEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<TerminalKeyEvent>);
    }
  }
}
