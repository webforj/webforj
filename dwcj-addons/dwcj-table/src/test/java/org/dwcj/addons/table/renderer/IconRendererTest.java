package org.dwcj.addons.table.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class IconRendererTest {

  @Test
  void shouldCreateRendererWithNameAndPool() {
    IconRenderer<String> renderer = new IconRenderer<>("testName", "testPool");
    assertEquals("testName", renderer.getName());
    assertEquals("testPool", renderer.getPool());
  }

  @Test
  void shouldCreateRendererWithName() {
    IconRenderer<String> renderer = new IconRenderer<>("testName");
    assertEquals("testName", renderer.getName());
  }

  @Test
  void shouldSetAndGetName() {
    IconRenderer<String> renderer = new IconRenderer<>("testName");
    renderer.setName("newName");
    assertEquals("newName", renderer.getName());
  }

  @Test
  void shouldSetAndGetPool() {
    IconRenderer<String> renderer = new IconRenderer<>("testName", "testPool");
    renderer.setPool("newPool");
    assertEquals("newPool", renderer.getPool());
  }
}
