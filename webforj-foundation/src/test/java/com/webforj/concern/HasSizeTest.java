package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasSizeTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetWidthAndHeight() {
    float width = 100;
    float minWidth = 50;
    float maxWidth = 200;
    float height = 200;
    float minHeight = 100;
    float maxHeight = 300;

    assertSame(component.setSize(width, height), component);
    assertSame(component.setMinSize(minWidth, minHeight), component);
    assertSame(component.setMaxSize(maxWidth, maxHeight), component);

    assertEquals(String.valueOf(width) + "px", component.getWidth());
    assertEquals(String.valueOf(width) + "px", component.getComputedWidth());
    assertEquals(String.valueOf(minWidth) + "px", component.getMinWidth());
    assertEquals(String.valueOf(minWidth) + "px", component.getComputedMinWidth());
    assertEquals(String.valueOf(maxWidth) + "px", component.getMaxWidth());
    assertEquals(String.valueOf(maxWidth) + "px", component.getComputedMaxWidth());

    assertEquals(String.valueOf(height) + "px", component.getHeight());
    assertEquals(String.valueOf(height) + "px", component.getComputedHeight());
    assertEquals(String.valueOf(minHeight) + "px", component.getMinHeight());
    assertEquals(String.valueOf(minHeight) + "px", component.getComputedMinHeight());
    assertEquals(String.valueOf(maxHeight) + "px", component.getMaxHeight());
    assertEquals(String.valueOf(maxHeight) + "px", component.getComputedMaxHeight());
  }

  class CompositeMock extends Composite<ConcernComponentMock> implements HasSize<CompositeMock> {
  }
}
