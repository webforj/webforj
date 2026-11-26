package com.webforj.component.button;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.Expanse;
import com.webforj.component.button.event.ButtonClickEvent;
import com.webforj.component.window.Window;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class ButtonTest {

  @Nested
  class Constructors {

    @Test
    void shouldCreateWithTextThemeAndClickListener() {
      Button button = new Button("Click Me", ButtonTheme.DEFAULT, e -> {
      });
      assertEquals("Click Me", button.getText());
      assertEquals(ButtonTheme.DEFAULT, button.getTheme());
      assertEquals(1, button.getEventListeners(ButtonClickEvent.class).size());
    }

    @Test
    void shouldCreateWithTextAndTheme() {
      Button button = new Button("Click Me", ButtonTheme.DEFAULT);
      assertEquals("Click Me", button.getText());
      assertEquals(ButtonTheme.DEFAULT, button.getTheme());
    }

    @Test
    void shouldCreateWithTextAndClickListener() {
      Button button = new Button("Click Me", e -> {
      });
      assertEquals("Click Me", button.getText());
      assertEquals(1, button.getEventListeners(ButtonClickEvent.class).size());
    }

    @Test
    void shouldCreateWithTextOnly() {
      Button button = new Button("Click Me");
      assertEquals("Click Me", button.getText());
    }

    @Test
    void shouldCreateWithNoArguments() {
      Button button = new Button();
      assertEquals("", button.getText());
    }
  }

  @Nested
  class Creation {

    @Test
    void shouldCreateControlWithTextWithoutSendingItAgainOnAttach()
        throws BBjException, IllegalAccessException {
      Window window = mock(Window.class);
      BBjWindow bbjWindow = mock(BBjWindow.class);
      BBjButton control = mock(BBjButton.class);
      WindowAccessor windowAccessor = mock(WindowAccessor.class);
      when(windowAccessor.getBBjWindow(window)).thenReturn(bbjWindow);
      when(bbjWindow.addButton(anyInt(), anyString(), any(byte[].class))).thenReturn(control);
      Button button = new Button("Click Me");

      try (MockedStatic<WindowAccessor> accessor = mockStatic(WindowAccessor.class)) {
        accessor.when(WindowAccessor::getDefault).thenReturn(windowAccessor);
        button.onCreate(window);
        button.onAttach();
      }

      verify(bbjWindow).addButton(anyInt(), eq("Click Me"), any(byte[].class));
      verify(control, never()).setText(anyString());
    }

    @Test
    void shouldSendTextChangedBetweenCreationAndAttach()
        throws BBjException, IllegalAccessException {
      Window window = mock(Window.class);
      BBjWindow bbjWindow = mock(BBjWindow.class);
      BBjButton control = mock(BBjButton.class);
      WindowAccessor windowAccessor = mock(WindowAccessor.class);
      when(windowAccessor.getBBjWindow(window)).thenReturn(bbjWindow);
      when(bbjWindow.addButton(anyInt(), anyString(), any(byte[].class))).thenReturn(control);
      Button button = new Button("Click Me");

      try (MockedStatic<WindowAccessor> accessor = mockStatic(WindowAccessor.class)) {
        accessor.when(WindowAccessor::getDefault).thenReturn(windowAccessor);
        button.onCreate(window);
        button.setText("Changed");
        button.onAttach();
      }

      verify(control, times(2)).setText("Changed");
      verify(control, never()).setText("Click Me");
    }

    @Test
    void shouldNotSendTheDefaultThemeOnAttach() throws BBjException, IllegalAccessException {
      BBjButton control = createAndAttach(new Button("Click Me"));

      verify(control, never()).setProperty(eq("theme"), any());
      verify(control).setProperty("expanse", Expanse.MEDIUM);
    }

    @Test
    void shouldSendThemeOtherThanTheDefaultOnAttach() throws BBjException, IllegalAccessException {
      BBjButton control = createAndAttach(new Button("Click Me", ButtonTheme.PRIMARY));

      verify(control).setProperty("theme", ButtonTheme.PRIMARY);
    }

    @Test
    void shouldNotSendTheDefaultThemeSetBackBeforeAttach()
        throws BBjException, IllegalAccessException {
      Button button = new Button("Click Me", ButtonTheme.DANGER);
      button.setTheme(ButtonTheme.DEFAULT);

      BBjButton control = createAndAttach(button);

      verify(control, never()).setProperty(eq("theme"), any());
    }

    @Test
    void shouldSendTheDefaultThemeSetAfterAttach() throws BBjException, IllegalAccessException {
      Button button = new Button("Click Me", ButtonTheme.DANGER);
      BBjButton control = createAndAttach(button);

      button.setTheme(ButtonTheme.DEFAULT);

      verify(control).setProperty("theme", ButtonTheme.DEFAULT);
    }

    @Test
    void shouldKeepTheDefaultThemeBeforeAttach() {
      Button button = new Button("Click Me");

      assertEquals(ButtonTheme.DEFAULT, button.getTheme());
      assertEquals("default", button.getProperty("theme"));
    }

    private BBjButton createAndAttach(Button button) throws BBjException, IllegalAccessException {
      Window window = mock(Window.class);
      BBjWindow bbjWindow = mock(BBjWindow.class);
      BBjButton control = mock(BBjButton.class);
      WindowAccessor windowAccessor = mock(WindowAccessor.class);
      when(windowAccessor.getBBjWindow(window)).thenReturn(bbjWindow);
      when(bbjWindow.addButton(anyInt(), anyString(), any(byte[].class))).thenReturn(control);

      try (MockedStatic<WindowAccessor> accessor = mockStatic(WindowAccessor.class)) {
        accessor.when(WindowAccessor::getDefault).thenReturn(windowAccessor);
        button.onCreate(window);
        button.onAttach();
      }

      return control;
    }
  }
}
