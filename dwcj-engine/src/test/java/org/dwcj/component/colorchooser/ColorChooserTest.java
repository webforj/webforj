package org.dwcj.component.colorchooser;

import com.basis.bbj.proxies.sysgui.BBjColorChooser;
import com.basis.startup.type.BBjException;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.commons.lang3.reflect.MethodUtils;
import org.dwcj.component.Component;
import org.dwcj.component.colorchooser.event.ColorChooserApproveEvent;
import org.dwcj.component.colorchooser.event.ColorChooserChangeEvent;
import org.dwcj.component.colorchooser.sink.ColorChooserApproveEventSink;
import org.dwcj.component.colorchooser.sink.ColorChooserCancelEventSink;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import java.awt.*;
import java.lang.reflect.InvocationTargetException;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;


/** ColorChooser tests. */
@ExtendWith(MockitoExtension.class)
public class ColorChooserTest {

  @Mock
  BBjColorChooser control;

  @Mock
  ColorChooserApproveEventSink colorChooserApproveEventSink;

  @Mock
  ColorChooserChangeEvent colorChooserChangeEvent;

  @Mock
  ColorChooserCancelEventSink colorChooserCancelEventSink;

  @Spy
  EventDispatcher dispatcher;

  @InjectMocks
  ColorChooser component;

  void nullifyControl() throws IllegalAccessException {
    FieldUtils.writeField(component, "control", null, true);
  }

  @DisplayName("Constructor")
  class Constructor {

    @Test
    @DisplayName("Constructor with color")
    void colorConstructor() {
      ColorChooser component = new ColorChooser(new Color(255, 0, 0));
      assertEquals(new Color(255, 0, 0), component.getColor());
    }
  }

  @Nested
  @DisplayName("CatchUp behavior")
  class CatchUp {
    void invokeCatchUp(ColorChooser component) throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
      MethodUtils.invokeMethod(component, true, "catchUp");
    }

    @Test
    @DisplayName("Calling it twice should not be allowed")
    void callingTwiceShouldNotBeAllowed() throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
      ColorChooser componentSpy = spy(component);
      invokeCatchUp(componentSpy);

      assertThrows(InvocationTargetException.class, () -> invokeCatchUp(componentSpy));
    }

    @Test
    @DisplayName("catchUp method")
    void catchUpMethod() throws NoSuchMethodException, IllegalAccessException, BBjException, InvocationTargetException {
      ColorChooser componentSpy = spy(component);
      componentSpy.setTabTraversable(true);
      componentSpy.onMouseEnter(e -> {});
      componentSpy.onMouseExit(e -> {});
      componentSpy.onRightMouseDown(e -> {});

      invokeCatchUp(componentSpy);

      verify(componentSpy, atLeast(2)).setVisible(true);
      verify(componentSpy, atLeast(2)).setTabTraversable(true);
    }
  }

  @Nested
  @DisplayName("Preview PanelVisible API")
  class PreviewPanelVisible {
    @Test
    @DisplayName("When control is defined")
    void whenControlIsDefined() throws BBjException {
      component.setPreviewPanelVisible(true);
      assertTrue(component.isPreviewPanelVisible());

      verify(control, times(1)).setPreviewPanelVisible(true);
      verify(control, times(0)).isPreviewPanelVisible();
    }

    @Test
    @DisplayName("When control is null")
    void whenControlIsNull() throws BBjException, IllegalAccessException {
      nullifyControl();
      component.setPreviewPanelVisible(false);
      assertFalse(component.isPreviewPanelVisible());
    }

    @Test
    @DisplayName("When control throws BBjException, DwcjRuntimeException")
    void reThrowDwcjRuntimeException() throws Exception {
      doThrow(BBjException.class).when(control).setPreviewPanelVisible(anyBoolean());
      assertThrows(DwcjRuntimeException.class, () -> component.setPreviewPanelVisible(false));
    }
  }

  @Nested
  @DisplayName("ApproveButtonText API")
  class ApproveButtonText {
    @Test
    @DisplayName("When control is defined")
    void whenControlIsDefined() throws BBjException {
      component.setApproveButtonText("approve!");
      assertEquals("approve!", component.getApproveButtonText());

      verify(control, times(1)).setApproveButtonText("approve!");
      verify(control, times(0)).getApproveButtonText();
    }

    @Test
    @DisplayName("When control is null")
    void whenControlIsNull() throws IllegalAccessException {
      nullifyControl();
      component.setApproveButtonText("approve pls");
      assertEquals("approve pls", component.getApproveButtonText());
    }

    @Test
    @DisplayName("When control throws BBjException, DwcjRuntimeException")
    void reThrowDwcjRuntimeException() throws Exception {
      doThrow(BBjException.class).when(control).setApproveButtonText(anyString());
      assertThrows(DwcjRuntimeException.class, () -> component.setApproveButtonText("approve"));
    }
  }

  @Nested
  @DisplayName("CancelButtonText API")
  class CancelButtonText {
    @Test
    @DisplayName("When control is defined")
    void whenControlIsDefined() throws BBjException {
      component.setCancelButtonText("cancel");
      assertEquals("cancel", component.getCancelButtonText());

      verify(control, times(1)).setCancelButtonText("cancel");
      verify(control, times(0)).getApproveButtonText();
    }

    @Test
    @DisplayName("When control is null")
    void whenControlIsNull() throws IllegalAccessException {
      nullifyControl();
      component.setCancelButtonText("cancel pls");
      assertEquals("cancel pls", component.getCancelButtonText());
    }

    @Test
    @DisplayName("When control throws BBjException, DwcjRuntimeException")
    void reThrowDwcjRuntimeException() throws Exception {
      doThrow(BBjException.class).when(control).setCancelButtonText(anyString());
      assertThrows(DwcjRuntimeException.class, () -> component.setCancelButtonText("cancel"));
    }
  }

  @Nested
  @DisplayName("ButtonsShown API")
  class ButtonsShown {
    @Test
    @DisplayName("When control is defined")
    void whenControlIsDefined() throws BBjException {
      component.setControlButtonsAreShown(false);
      assertFalse(component.getControlButtonsAreShown());
      verify(control, times(1)).setControlButtonsAreShown(false);
      verify(control, times(0)).getControlButtonsAreShown();
    }

    @Test
    @DisplayName("When control is null")
    void whenControlIsNull() throws IllegalAccessException {
      nullifyControl();
      component.setControlButtonsAreShown(false);
      assertFalse(component.getControlButtonsAreShown());
    }

    @Test
    @DisplayName("When control throws BBjException, DwcjRuntimeException")
    void reThrowDwcjRuntimeException() throws Exception {
      doThrow(BBjException.class).when(control).setControlButtonsAreShown(anyBoolean());
      assertThrows(DwcjRuntimeException.class, () -> component.setControlButtonsAreShown(true));
    }
  }

  @Nested
  @DisplayName("Approve Selection API")
  class ApproveSelection {
    @Test
    @DisplayName("When control is defined")
    void whenControlIsDefined() throws BBjException {
      component.approveSelection();
      verify(control, times(0)).approveSelection();
    }

    @Test
    @DisplayName("When control throws BBjException, DwcjRuntimeException")
    void reThrowDwcjRuntimeException() throws Exception {
      doThrow(BBjException.class).when(control).approveSelection();
      assertThrows(DwcjRuntimeException.class, () -> component.approveSelection());
    }
  }

  @Nested
  @DisplayName("Cancel Selection API")
  class CancelSelection {
    @Test
    @DisplayName("When control is defined")
    void whenControlIsDefined() throws BBjException {
      component.cancelSelection();
      verify(control, times(0)).cancelSelection();
    }

    @Test
    @DisplayName("When control throws BBjException, DwcjRuntimeException")
    void reThrowDecjRuntimeException() throws Exception {
      doThrow(BBjException.class).when(control).cancelSelection();
      assertThrows(DwcjRuntimeException.class, () -> component.cancelSelection());
    }
  }

  @Nested
  @DisplayName("Approve Events")
  class Approve {
    @Test
    @DisplayName("AddListener when control is defined")
    void addListenerWhenControlIsDefined()  {
      EventListener<ColorChooserApproveEvent> listener = e -> {
//        do nothing
      };

      component.onApprove(listener);
      verify(colorChooserApproveEventSink, times(1)).setCallback();
      verify(dispatcher, times(1)).addEventListener(ColorChooserApproveEvent.class, listener);
    }

    @Test
    @DisplayName("AddListener when control is null")
    void addListenerWhenControlIsNull() throws IllegalAccessException {
      nullifyControl();
      EventListener<ColorChooserApproveEvent> listener = e -> {
        // do nothing
      };

      component.onApprove(listener);
      verify(colorChooserApproveEventSink, times(0)).setCallback();
      verify(dispatcher, times(1)).addEventListener(ColorChooserApproveEvent.class, listener);
    }

    @Test
    @DisplayName("When dispatch has already ColorChooserApproveEventListener registered")
    void whenDispatchHasAlreadyColoChooserApproveEventListenerRegistered() {
      EventListener<ColorChooserApproveEvent> listener = e -> {
        // do nothing
      };
      verify(colorChooserApproveEventSink, times(0)).setCallback();
    }

    @Test
    @DisplayName("RemoveListener when control is defined")
    void removeListenerWhenControlIsDefined() {
      EventListener<ColorChooserApproveEvent> listener = e -> {
        // do nothing
      };
      component.onApprove(listener);
      component.removeColorChooserApproveListener(listener);

      verify(colorChooserApproveEventSink, times(1)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(ColorChooserApproveEvent.class, listener);
    }

    @Test
    @DisplayName("removeListener when control is null")
    void removeListenerWhenControlIsNull() throws IllegalAccessException {
      nullifyControl();
      EventListener<ColorChooserApproveEvent> listener = e -> {
        // do nothing
      };
      component.onApprove(listener);
      component.removeColorChooserApproveListener(listener);

      verify(colorChooserApproveEventSink, times(0)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(ColorChooserApproveEvent.class, listener);
    }

    @Test
    @DisplayName("removeListener when dispatcher has alraedy more than one ColorChooserApproveEventListener registered")
    void removeListenerWhenDispatcherHasAlreadyMoreThanOneColorChooserApproveEventListenerRegistered() {
      EventListener<ColorChooserApproveEvent> listener = e -> {
        // do nothing
      };
      EventListener<ColorChooserApproveEvent> listener2 = e -> {
        // do nothing
      };

      component.onApprove(listener);
      component.onApprove(listener2);
      assertEquals(2, dispatcher.getListenersCount(ColorChooserApproveEvent.class));

      component.removeColorChooserApproveListener(listener);
      verify(colorChooserApproveEventSink, times(0)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(ColorChooserApproveEvent.class, listener);
    }

  }

}



















