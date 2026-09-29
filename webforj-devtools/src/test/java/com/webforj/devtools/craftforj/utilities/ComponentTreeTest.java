package com.webforj.devtools.craftforj.utilities;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.webforj.App;
import com.webforj.component.Component;
import com.webforj.component.Composite;
import com.webforj.component.button.Button;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.component.window.Frame;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

@DisplayName("ComponentTree")
class ComponentTreeTest {

  static class Card extends Composite<FlexLayout> {
    private final Button action = new Button("Action");

    Card() {
      getBoundComponent().add(action);
    }
  }

  @Test
  @DisplayName("should find what a component holds, a parent before its children")
  void shouldFindBelow() {
    Button save = new Button("Save");
    Card card = new Card();
    FlexLayout row = new FlexLayout(save, card);
    FlexLayout layout = new FlexLayout(row);

    List<Component> below = ComponentTree.findBelow(layout);

    assertEquals(
        List.of("Element", "FlexLayout", "Element", "Button", "Card", "FlexLayout", "Element",
            "Button"),
        below.stream().map(component -> component.getClass().getSimpleName()).toList());
    assertSame(row, below.get(1));
    assertSame(save, below.get(3));
    assertSame(card, below.get(4));
    assertSame(card.action, below.get(7));
  }

  @Test
  @DisplayName("should leave out a destroyed component and what it holds")
  void shouldLeaveOutDestroyedComponents() {
    Button kept = new Button("Kept");
    Button held = new Button("Held");
    FlexLayout destroyed = mock(FlexLayout.class);
    when(destroyed.isDestroyed()).thenReturn(true);
    when(destroyed.getComponents()).thenReturn(List.of(held));
    Frame frame = mock(Frame.class);
    when(frame.getComponents()).thenReturn(List.of(destroyed, kept));

    try (MockedStatic<App> app = mockStatic(App.class)) {
      app.when(App::getFrames).thenReturn(List.of(frame));

      assertEquals(List.of(frame, kept), ComponentTree.findAll());
      assertTrue(ComponentLocator.findById(held.getComponentId()).isEmpty());
    }
  }

  @Test
  @DisplayName("should find the components of every frame and one of them by its id")
  void shouldFindAllAndById() {
    Button save = new Button("Save");
    FlexLayout layout = new FlexLayout(save);
    Frame frame = mock(Frame.class);
    when(frame.getComponents()).thenReturn(List.of(layout));

    try (MockedStatic<App> app = mockStatic(App.class)) {
      app.when(App::getFrames).thenReturn(List.of(frame));

      List<Component> all = ComponentTree.findAll();

      assertEquals(4, all.size());
      assertSame(frame, all.get(0));
      assertSame(layout, all.get(1));
      assertSame(save, all.get(3));
      assertSame(save, ComponentLocator.findById(save.getComponentId()).orElseThrow());
      assertTrue(ComponentLocator.findById("missing").isEmpty());
      assertTrue(ComponentLocator.findById("").isEmpty());
      assertTrue(ComponentLocator.findById(null).isEmpty());
    }
  }
}
