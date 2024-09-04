package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;

import com.webforj.component.Component;
import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasPrefixAndSuffixTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetPattern() {
    Component expectedPattern = mock(Component.class);
    assertSame(component, component.setPrefixComponent(expectedPattern));
    assertEquals(expectedPattern, component.getPrefixComponent());

    Component expectedSuffix = mock(Component.class);
    assertSame(component, component.setSuffixComponent(expectedSuffix));
    assertEquals(expectedSuffix, component.getSuffixComponent());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasPrefixAndSuffix<CompositeMock> {
  }
}
