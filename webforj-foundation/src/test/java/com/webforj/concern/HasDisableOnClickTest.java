package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.Component;
import com.webforj.component.Composite;
import com.webforj.component.window.Window;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasDisableOnClickTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetDisableOnClick() {
    assertFalse(component.isDisableOnClick());

    assertSame(component, component.setDisableOnClick(true));
    assertTrue(component.isDisableOnClick());

    component.setDisableOnClick(false);
    assertFalse(component.isDisableOnClick());
  }

  @Test
  void shouldThrowWhenBoundComponentDoesNotSupportDisableOnClick() {
    UnsupportedCompositeMock unsupported = new UnsupportedCompositeMock();

    assertThrows(UnsupportedOperationException.class, () -> unsupported.setDisableOnClick(true));
    assertThrows(UnsupportedOperationException.class, unsupported::isDisableOnClick);
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasDisableOnClick<CompositeMock> {
  }

  class UnsupportedCompositeMock extends Composite<PlainComponentMock>
      implements HasDisableOnClick<UnsupportedCompositeMock> {
  }

  static class PlainComponentMock extends Component {
    @Override
    protected void onCreate(Window window) {
      // pass
    }

    @Override
    protected void onDestroy() {
      // pass
    }
  }
}
