package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.table.event.renderer.RendererClickEvent;
import com.webforj.dispatcher.EventListener;
import org.junit.jupiter.api.Test;

class NativeButtonRendererTest {

  @Test
  void shouldCreateRendererWithContentAndListener() {
    EventListener<RendererClickEvent<String>> listener = event -> {
    };
    NativeButtonRenderer<String> renderer = new NativeButtonRenderer<>("testContent", listener);
    assertEquals("testContent", renderer.getContent());
  }

  @Test
  void shouldCreateRendererWithContent() {
    NativeButtonRenderer<String> renderer = new NativeButtonRenderer<>("testContent");
    assertEquals("testContent", renderer.getContent());
  }

  @Test
  void shouldCreateRendererWithoutContent() {
    NativeButtonRenderer<String> renderer = new NativeButtonRenderer<>();
    assertEquals(null, renderer.getContent());
  }

  @Test
  void shouldSetAndCheckEnabled() {
    NativeButtonRenderer<String> renderer = new NativeButtonRenderer<>();
    renderer.setEnabled(true);
    assertFalse(renderer.isEnabled());
    renderer.setEnabled(false);
    assertTrue(renderer.isEnabled());
  }
}
