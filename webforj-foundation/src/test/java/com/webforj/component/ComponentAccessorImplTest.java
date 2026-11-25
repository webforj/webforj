package com.webforj.component;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.window.Window;
import com.webforj.exceptions.WebforjRuntimeException;
import java.lang.reflect.InvocationTargetException;
import org.example.accessor.ForeignAccessorCaller;
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

  @Test
  void shouldRejectCreateFromCallerOutsideWebforjPackage() {
    Component component = mock(Component.class);
    Window window = mock(Window.class);

    assertThrows(IllegalAccessException.class,
        () -> ForeignAccessorCaller.create(componentAccessor, component, window));
    verify(component, never()).create(window);
  }

  @Test
  void shouldRejectGetControlFromCallerOutsideWebforjPackage() {
    DwcComponent<?> component = spy(new DwcComponentMock());

    assertThrows(IllegalAccessException.class,
        () -> ForeignAccessorCaller.getControl(componentAccessor, component));
  }

  @Test
  @SuppressWarnings("removal")
  void shouldRejectGetBBjControlFromCallerOutsideWebforjPackage() {
    LegacyDwcComponent component = mock(LegacyDwcComponent.class);

    assertThrows(IllegalAccessException.class,
        () -> ForeignAccessorCaller.getBBjControl(componentAccessor, component));
  }

  @Test
  void shouldGetControlOfEachCompositeBoundComponent() throws IllegalAccessException {
    CompositeMock first = new SubCompositeMock();
    CompositeMock second = new CompositeMock();

    BBjControl firstControl = componentAccessor.getControl(first);
    BBjControl secondControl = componentAccessor.getControl(second);

    assertSame(first.el.getControl(), firstControl);
    assertSame(second.el.getControl(), secondControl);
    assertNotSame(firstControl, secondControl);
    assertSame(firstControl, componentAccessor.getControl(first));
  }

  @Test
  void shouldWrapExceptionThrownByCreate() {
    Component component = mock(Component.class);
    Window window = mock(Window.class);
    doThrow(new IllegalStateException("boom")).when(component).create(window);

    WebforjRuntimeException exception = assertThrows(WebforjRuntimeException.class,
        () -> componentAccessor.create(component, window));

    assertTrue(exception.getCause() instanceof InvocationTargetException);
  }

  @Test
  void shouldThrowWhenCreatingNullComponent() {
    Window window = mock(Window.class);

    assertThrows(NullPointerException.class, () -> componentAccessor.create(null, window));
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
