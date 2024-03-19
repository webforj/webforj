package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasHtmlTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetHtml() {
    assertSame(component.setHtml("Hello, World!"), component);
    assertEquals("Hello, World!", component.getHtml());
  }

  class CompositeMock extends Composite<ConcernComponentMock> implements HasHtml<CompositeMock> {
  }
}
