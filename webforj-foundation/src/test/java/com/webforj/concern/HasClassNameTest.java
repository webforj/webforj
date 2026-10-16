package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.component.Component;
import com.webforj.component.Composite;
import com.webforj.component.window.Window;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasClassNameTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldAddClassName() {
    assertSame(component.addClassName("testClass"), component);
    assertEquals("testClass", component.getClassName());
  }

  @Test
  void shouldRemoveClassName() {
    component.addClassName("testClass");
    assertEquals("testClass", component.getClassName());

    component.removeClassName("testClass");
    assertNull(component.getClassName());
  }

  @Test
  void shouldGetClassNames() {
    component.addClassName("testClass");

    assertEquals(List.of("testClass"), component.getClassNameList());
  }

  @Test
  void shouldThrowWhenBoundComponentDoesNotSupportClassNames() {
    UnsupportedCompositeMock unsupported = new UnsupportedCompositeMock();

    assertThrows(UnsupportedOperationException.class, unsupported::getClassNameList);
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasClassName<CompositeMock> {

    public String getClassName() {
      return getBoundComponent().getClassName();
    }
  }

  class UnsupportedCompositeMock extends Composite<PlainComponentMock>
      implements HasClassName<UnsupportedCompositeMock> {
  }

  static class PlainComponentMock extends Component {
    @Override
    protected void onCreate(Window window) {}

    @Override
    protected void onDestroy() {}
  }
}
