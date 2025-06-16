package com.webforj.component;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.window.Window;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ComponentAccessorImplTest {
  ComponentAccessor componentAccessor;

  @BeforeEach
  void setUp() {
    componentAccessor = new ComponentAccessorImpl();
  }

  @Test
  @DisplayName("Should create component")
  void shouldCreateComponent() throws IllegalAccessException {
    Component component = mock(Component.class);
    Window window = mock(Window.class);
    componentAccessor.create(component, window);

    verify(component).create(window);
  }

  @Test
  void shouldTrackControls() throws IllegalAccessException, BBjException {
    class ComponentMock extends Composite<DwcComponentMock> {
      private DwcComponentMock el;

      @Override
      protected DwcComponentMock initBoundComponent() {
        el = spy(new DwcComponentMock());
        when(el.isDestroyed()).thenReturn(false);
        BBjControl controlMock = mock(BBjControl.class);
        try {
          when(controlMock.getUserData()).thenReturn(el);
        } catch (BBjException e) {
          // pass
        }
        when(el.getControl()).thenReturn(controlMock);
        return el;
      }
    }

    Component component = new ComponentMock();
    BBjControl control = componentAccessor.getControl(component);

    assertNotNull(control);
    assertTrue(control.getUserData() instanceof DwcComponent);
  }

  @Test
  @DisplayName("Should get control from DwcComponent")
  void shouldGetControlFromDwcComponent() throws IllegalAccessException {
    DwcComponent<?> component = spy(new DwcComponentMock());
    BBjControl mockControl = mock(BBjControl.class);
    when(component.getControl()).thenReturn(mockControl);

    BBjControl underlyingControl = componentAccessor.getControl(component);
    ((DwcComponent<?>) verify(component)).getControl();
    assertEquals(mockControl, underlyingControl);
  }

  @Test
  @DisplayName("Should get control from Composite")
  void shouldGetControlFromComposite() throws IllegalAccessException {
    Composite<DwcComponentMock> component = new SubCompositeMock();
    BBjControl underlyingControl = componentAccessor.getControl(component);
    assertNotNull(underlyingControl);
  }

  @Test
  @DisplayName("Should throw exception when there are no underlying control")
  void shouldThrowExceptionWhenNoControl() {
    Component component = mock(Component.class);
    assertThrows(IllegalAccessException.class, () -> componentAccessor.getControl(component));
  }

  public static class CompositeMock extends Composite<DwcComponentMock> {
    private DwcComponentMock el;

    @Override
    protected DwcComponentMock initBoundComponent() {
      el = spy(new DwcComponentMock());
      when(el.isDestroyed()).thenReturn(false);
      when(el.getControl()).thenReturn(spy(BBjControl.class));
      return el;
    }
  }

  public static class SubCompositeMock extends CompositeMock {
  }
}
