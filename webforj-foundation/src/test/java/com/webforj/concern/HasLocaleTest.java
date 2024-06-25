package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import java.util.Locale;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasLocaleTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetLabel() {
    Locale expectedLocale = Locale.CANADA;
    assertSame(component.setLocale(expectedLocale), component);
    assertEquals(expectedLocale, component.getLocale());
  }

  class CompositeMock extends Composite<ConcernComponentMock> implements HasLocale<CompositeMock> {
  }
}
