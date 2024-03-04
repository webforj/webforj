package com.webforj.component;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.element.Element;
import com.webforj.component.window.Window;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class CompositeTest {
  private CompositeMock component;

  @BeforeEach
  void setUp() {
    component = spy(new CompositeMock());
  }

  @Test
  @DisplayName("initBoundComponent will create Bound Component")
  void initBoundComponentWillCreateBoundComponent() {
    assertTrue(component.getBoundComponent() instanceof Element);
  }

  @Test
  @DisplayName("Bound Component will be initialized on creation")
  void boundComponentWillInitializeOnCreation() {
    Window window = mock(Window.class);
    component.onCreate(window);

    verify(window, times(1)).add(any(Element.class));
  }

  @Test
  @DisplayName("Composite fails to instantiate Bound Component without No-Arg Constructor")
  void compositeCannotCreateBoundComponentAutomatically() {

    Throwable exp = assertThrows(IllegalStateException.class, () -> {
      new Composite<NoArgConstructorComponent>() {};
    });

    assertTrue(exp.getMessage().contains("does not have a no-arg constructor"));
  }

  @Test
  @DisplayName("Bound Component will be destroyed on destruction")
  void testOnDestroy() {

    component.destroy();
    verify(component.getBoundComponent(), times(1)).destroy();
    assertTrue(component.isDestroyed());
  }

  public static class CompositeMock extends Composite<Element> {
    private Element el;

    @Override
    protected Element initBoundComponent() {
      el = mock(Element.class);
      when(el.isDestroyed()).thenReturn(false);
      return el;
    }

    @Override
    protected void onDestroy() {
      super.onDestroy();
      when(el.isDestroyed()).thenReturn(true);
    }
  }

  private static class NoArgConstructorComponent extends Composite<Element> {

    public NoArgConstructorComponent(String arg) {
      // pass
    }
  }
}
