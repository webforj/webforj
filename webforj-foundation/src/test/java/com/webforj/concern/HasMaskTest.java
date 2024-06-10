package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasMaskTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetMask() {
    String expectedMask = "XX-XX-XX";
    assertSame(component, component.setMask(expectedMask));
    assertEquals(expectedMask, component.getMask());
  }

  class CompositeMock extends Composite<ConcernComponentMock> implements HasMask<CompositeMock> {
  }
}
