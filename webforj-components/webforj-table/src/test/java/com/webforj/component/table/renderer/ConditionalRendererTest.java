package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import com.webforj.component.table.event.renderer.RendererChangeEvent;
import com.webforj.dispatcher.EventListener;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class ConditionalRendererTest {

  ConditionalRenderer<String> renderer;

  @BeforeEach
  void setup() {
    renderer = new ConditionalRenderer<>();
  }

  @Test
  void shouldBuildEmptyRenderer() {
    assertEquals("", renderer.build());
  }

  @Test
  void shouldBuildWithFallbackOnly() {
    Renderer<String> fallback = createMockRenderer("fallback-output");
    renderer.otherwise(fallback);
    assertEquals("fallback-output", renderer.build());
  }

  @Test
  void shouldBuildSingleCondition() {
    Renderer<String> rockRenderer = createMockRenderer("<dwc-badge theme='primary'></dwc-badge>");
    renderer.when("Rock", rockRenderer);

    String result = renderer.build();
    assertTrue(result.contains("<% if (String(cell.value) === 'Rock') { %>"));
    assertTrue(result.contains("<dwc-badge theme='primary'></dwc-badge>"));
    assertTrue(result.contains("<% } %>"));
  }

  @Test
  void shouldBuildMultipleConditions() {
    Renderer<String> rockRenderer = createMockRenderer("<rock/>");
    Renderer<String> jazzRenderer = createMockRenderer("<jazz/>");
    renderer.when("Rock", rockRenderer);
    renderer.when("Jazz", jazzRenderer);

    String result = renderer.build();
    assertTrue(result.contains("<% if (String(cell.value) === 'Rock') { %>"));
    assertTrue(result.contains("<rock/>"));
    assertTrue(result.contains("<% } else if (String(cell.value) === 'Jazz') { %>"));
    assertTrue(result.contains("<jazz/>"));
    assertTrue(result.contains("<% } %>"));
  }

  @Test
  void shouldBuildConditionsWithFallback() {
    Renderer<String> rockRenderer = createMockRenderer("<rock/>");
    Renderer<String> fallback = createMockRenderer("<default/>");
    renderer.when("Rock", rockRenderer);
    renderer.otherwise(fallback);

    String result = renderer.build();
    assertTrue(result.contains("<% if (String(cell.value) === 'Rock') { %>"));
    assertTrue(result.contains("<rock/>"));
    assertTrue(result.contains("<% } else { %>"));
    assertTrue(result.contains("<default/>"));
    assertTrue(result.contains("<% } %>"));
  }

  @Test
  void shouldBuildWithConditionObject() {
    Renderer<String> highRenderer = createMockRenderer("<high/>");
    renderer.when(Condition.greaterThan(100), highRenderer);

    String result = renderer.build();
    assertTrue(result.contains("<% if (Number(cell.value) > 100) { %>"));
    assertTrue(result.contains("<high/>"));
  }

  @Test
  void shouldBuildWithInCondition() {
    Renderer<String> matched = createMockRenderer("<matched/>");
    renderer.when(Condition.in("Rock", "Jazz"), matched);

    String result = renderer.build();
    assertTrue(result.contains("['Rock','Jazz'].indexOf(String(cell.value)) !== -1"));
    assertTrue(result.contains("<matched/>"));
  }

  @Test
  void shouldBuildWithComposedCondition() {
    Renderer<String> rangeRenderer = createMockRenderer("<range/>");
    renderer.when(Condition.greaterThan(10).and(Condition.lessThan(50)), rangeRenderer);

    String result = renderer.build();
    assertTrue(result.contains("(Number(cell.value) > 10) && (Number(cell.value) < 50)"));
  }

  @Test
  void shouldBuildFullChain() {
    renderer.when("Rock", createMockRenderer("<rock/>")).when("Jazz", createMockRenderer("<jazz/>"))
        .when(Condition.in("Pop", "R&B"), createMockRenderer("<pop/>"))
        .otherwise(createMockRenderer("<other/>"));

    String result = renderer.build();
    assertTrue(result.contains("<% if ("));
    assertTrue(result.contains("<% } else if ("));
    assertTrue(result.contains("<% } else { %>"));
    assertTrue(result.contains("<rock/>"));
    assertTrue(result.contains("<jazz/>"));
    assertTrue(result.contains("<pop/>"));
    assertTrue(result.contains("<other/>"));
    assertTrue(result.endsWith("<% } %>"));
  }

  @Test
  @SuppressWarnings("unchecked")
  void shouldFireChangeEventWhenConditionAdded() {
    EventListener<RendererChangeEvent> listener = mock(EventListener.class);
    renderer.addChangeListener(listener);

    renderer.when("Rock", createMockRenderer("<rock/>"));
    verify(listener).onEvent(any(RendererChangeEvent.class));
  }

  @Test
  @SuppressWarnings("unchecked")
  void shouldFireChangeEventWhenFallbackSet() {
    EventListener<RendererChangeEvent> listener = mock(EventListener.class);
    renderer.addChangeListener(listener);

    renderer.otherwise(createMockRenderer("<default/>"));
    verify(listener).onEvent(any(RendererChangeEvent.class));
  }

  @Test
  @SuppressWarnings("unchecked")
  void shouldPropagateSubRendererChangeEvents() {
    EventListener<RendererChangeEvent> listener = mock(EventListener.class);
    renderer.addChangeListener(listener);

    Renderer<String> subRenderer = new Renderer<>() {
      @Override
      public String build() {
        return "<sub/>";
      }
    };

    renderer.when("Rock", subRenderer);
    // Change event from when() call
    verify(listener).onEvent(any(RendererChangeEvent.class));

    // Modify sub-renderer — should propagate
    subRenderer.setAttribute("foo", "bar");
    verify(listener, org.mockito.Mockito.atLeast(2)).onEvent(any(RendererChangeEvent.class));
  }

  private Renderer<String> createMockRenderer(String output) {
    return new Renderer<>() {
      @Override
      public String build() {
        return output;
      }
    };
  }
}
