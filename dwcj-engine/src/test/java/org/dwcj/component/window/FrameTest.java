package org.dwcj.component.window;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjTopLevelWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.component.window.Frame.Area;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class FrameTest {

  BBjTopLevelWindow bbjWindow;
  Frame frame;

  @BeforeEach
  void setup() {
    bbjWindow = mock(BBjTopLevelWindow.class);
    frame = new Frame(bbjWindow);
  }

  @Test
  void shouldThrowExceptionIfFrameAddedToFrame() {
    Frame frame2 = new Frame(bbjWindow);

    assertThrows(IllegalArgumentException.class, () -> {
      frame.add(frame2);
    });
  }

  @Nested
  class StylesApi {

    @ParameterizedTest
    @EnumSource(Area.class)
    void shouldSetStyle(Area area) throws BBjException {
      frame.setStyle("background-color", "red", area);

      switch (area) {
        case WINDOW:
          verify(bbjWindow).setOuterStyle("background-color", "red");
          break;
        case CENTER:
          verify(bbjWindow).setStyle("background-color", "red");
          break;

        case CONTENT:
          verify(bbjWindow).setPanelStyle("background-color", "red");
          break;
        default:
          fail("Unexpected area: " + area);
          break;
      }
    }

    @Test
    void shouldSetContentStyleByDefault() throws BBjException {
      frame.setStyle("background-color", "red");
      verify(bbjWindow).setPanelStyle("background-color", "red");
    }

    @Test
    void shouldThrowExceptionFromSetStyle() throws BBjException {
      doThrow(BBjException.class).when(bbjWindow).setPanelStyle(anyString(), anyString());

      assertThrows(DwcjRuntimeException.class, () -> {
        frame.setStyle("background-color", "red");
      });
    }

    @ParameterizedTest
    @EnumSource(Area.class)
    void shouldGetStyle(Area area) throws BBjException {
      frame.getStyle("background-color", area);

      switch (area) {
        case WINDOW:
          verify(bbjWindow).getOuterStyle("background-color");
          break;
        case CENTER:
          verify(bbjWindow).getStyle("background-color");
          break;

        case CONTENT:
          verify(bbjWindow).getPanelStyle("background-color");
          break;
        default:
          fail("Unexpected area: " + area);
          break;
      }
    }

    @Test
    void shouldGetContentStyleByDefault() throws BBjException {
      frame.getStyle("background-color");
      verify(bbjWindow).getPanelStyle("background-color");
    }

    @Test
    void shouldThrowExceptionFromGetStyle() throws BBjException {
      when(bbjWindow.getPanelStyle(anyString())).thenThrow(BBjException.class);

      assertThrows(DwcjRuntimeException.class, () -> {
        frame.getStyle("background-color");
      });
    }

    @Test
    void shouldGetComputedStyle() throws BBjException {
      frame.getComputedStyle("background-color");
      verify(bbjWindow).getComputedPanelStyle("background-color");
    }

    @Test
    void shouldThrowExceptionFromGetComputedStyle() throws BBjException {
      when(bbjWindow.getComputedPanelStyle(anyString())).thenThrow(BBjException.class);

      assertThrows(DwcjRuntimeException.class, () -> {
        frame.getComputedStyle("background-color");
      });
    }
  }

  @Nested
  class ClassNamesApi {

    @ParameterizedTest
    @EnumSource(Area.class)
    void shouldSetClass(Area area) throws BBjException {
      frame.addClassName(area, "my-class");

      switch (area) {
        case WINDOW:
          verify(bbjWindow).addOuterClass("my-class");
          break;
        case CENTER:
          verify(bbjWindow).addClass("my-class");
          break;

        case CONTENT:
          verify(bbjWindow).addPanelClass("my-class");
          break;
        default:
          fail("Unexpected area: " + area);
          break;
      }
    }

    @Test
    void shouldSetContentClassByDefault() throws BBjException {
      frame.addClassName("my-class");
      verify(bbjWindow).addPanelClass("my-class");
    }

    @Test
    void shouldThrowExceptionFromAddClassName() throws BBjException {
      doThrow(BBjException.class).when(bbjWindow).addPanelClass(anyString());

      assertThrows(DwcjRuntimeException.class, () -> {
        frame.addClassName("my-class");
      });
    }

    @ParameterizedTest
    @EnumSource(Area.class)
    void shouldRemoveClass(Area area) throws BBjException {
      frame.removeClassName(area, "my-class");

      switch (area) {
        case WINDOW:
          verify(bbjWindow).removeOuterClass("my-class");
          break;
        case CENTER:
          verify(bbjWindow).removeClass("my-class");
          break;

        case CONTENT:
          verify(bbjWindow).removePanelClass("my-class");
          break;
        default:
          fail("Unexpected area: " + area);
          break;
      }
    }

    @Test
    void shouldRemoveContentClassByDefault() throws BBjException {
      frame.removeClassName("my-class");
      verify(bbjWindow).removePanelClass("my-class");
    }

    @Test
    void shouldThrowExceptionFromRemoveClassName() throws BBjException {
      doThrow(BBjException.class).when(bbjWindow).removePanelClass(anyString());

      assertThrows(DwcjRuntimeException.class, () -> {
        frame.removeClassName("my-class");
      });
    }
  }
}
