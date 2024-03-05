package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import com.webforj.component.table.Table;
import com.webforj.component.table.event.renderer.RendererChangeEvent;
import com.webforj.component.table.event.renderer.RendererClickEvent;
import com.webforj.component.table.renderer.AbstractVoidElementRenderer;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class AbstractVoidElementRendererTest {

  AbstractVoidElementRenderer<String> renderer;
  Table<String> table;
  EventListener<RendererChangeEvent> changeListener;

  @BeforeEach
  void setup() {
    table = new Table<>();
    renderer = new AbstractVoidElementRenderer<String>() {
      @Override
      public String build() {
        return "mock";
      }
    };
    changeListener = mock(EventListener.class);
    renderer.onChanged(changeListener);
  }

  @Test
  void shouldSetAndGetContent() {
    renderer.setContent("testContent");
    assertEquals("testContent", renderer.getContent());
    verify(changeListener).onEvent(any(RendererChangeEvent.class));
  }

  @Test
  void shouldThrowExceptionWhenTagNameIsNotAnnotated() {
    assertThrows(WebforjRuntimeException.class, () -> renderer.getNodeName());
  }

  @Test
  void shouldAddClickListener() {
    EventListener<RendererClickEvent<String>> clickListener = mock(EventListener.class);
    ListenerRegistration<RendererClickEvent<String>> registration = renderer.onClick(clickListener);
    assertNotNull(registration);
  }
}
