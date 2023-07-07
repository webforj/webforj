package org.dwcj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.startup.type.BBjException;
import java.lang.reflect.InvocationTargetException;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.commons.lang3.reflect.MethodUtils;
import org.dwcj.component.event.BlurEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.FocusEvent;
import org.dwcj.component.event.KeypressEvent;
import org.dwcj.component.event.ModifyEvent;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.MouseExitEvent;
import org.dwcj.component.event.RightMouseDownEvent;
import org.dwcj.component.event.ToggleEvent;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class AbstractFieldTest {

  @Mock
  BBjEditBox control;

  @InjectMocks
  AbstractFieldMock component;

  void nullifyControl() throws IllegalAccessException {
    FieldUtils.writeField(component, "control", null, true);
  }

  @Test
  @DisplayName("AutoComplete")
  void autoComplete() throws BBjException {
    component.setAutoComplete(AbstractFieldMock.Autocomplete.ON);
    assertEquals(AbstractFieldMock.Autocomplete.ON, component.getAutoComplete());

    verify(control, times(1)).putClientProperty("autocomplete", "on");
  }

  @Test
  @DisplayName("Label")
  void label() throws BBjException {
    component.setLabel("label");
    assertEquals("label", component.getLabel());

    verify(control, times(1)).putClientProperty("label", "label");
  }

  @Test
  @DisplayName("Required")
  void required() throws BBjException {
    component.setRequired(true);
    assertEquals(true, component.isRequired());

    verify(control, times(1)).putClientProperty("required", true);
  }

  @Test
  @DisplayName("SpellCheck")
  void spellCheck() throws BBjException {
    component.setSpellCheck(true);
    assertEquals(true, component.isSpellCheck());

    verify(control, times(1)).putClientProperty("spellcheck", true);
  }

  @Nested
  @DisplayName("Focus API")
  class FocusApi {
    @Test
    @DisplayName("hasFocus when control is defined")
    void hasFocusWhenControlIsDefined() throws BBjException {
      doReturn("true").when(control).getClientProperty("hasFocus");
      assertTrue(component.hasFocus());
    }

    @Test
    @DisplayName("hasFocus when control is null")
    void hasFocusWhenControlIsNull() throws BBjException, IllegalAccessException {
      nullifyControl();
      assertFalse(component.hasFocus());
    }

    @Test
    @DisplayName("AutoFocus")
    void autoFocus() throws BBjException {
      component.setAutoFocus(true);
      assertEquals(true, component.isAutoFocus());

      verify(control, times(1)).putClientProperty("autofocus", true);
    }
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    @DisplayName("adding/removing supported events")
    void addingRemovingSupportedEvents() {
      EventListener<ModifyEvent> modifyListener = event -> {
      };
      EventListener<KeypressEvent> keypressListener = event -> {
      };
      EventListener<MouseEnterEvent> mouseEnterListener = event -> {
      };
      EventListener<FocusEvent> focusListener = event -> {
      };
      EventListener<BlurEvent> blurListener = event -> {
      };
      EventListener<MouseExitEvent> mouseExitListener = event -> {
      };
      EventListener<RightMouseDownEvent> rightMouseDownListener = event -> {
      };

      component.onModify(modifyListener);
      component.onKeypress(keypressListener);
      component.onFocus(focusListener);
      component.onBlur(blurListener);
      component.onMouseEnter(mouseEnterListener);
      component.onMouseExit(mouseExitListener);
      component.onRightMouseDown(rightMouseDownListener);

      EventDispatcher dispatcher = component.getEventDispatcher();

      assertEquals(1, dispatcher.getListenersCount(ModifyEvent.class));
      assertEquals(1, dispatcher.getListenersCount(KeypressEvent.class));
      assertEquals(1, dispatcher.getListenersCount(FocusEvent.class));
      assertEquals(1, dispatcher.getListenersCount(BlurEvent.class));
      assertEquals(1, dispatcher.getListenersCount(MouseEnterEvent.class));
      assertEquals(1, dispatcher.getListenersCount(MouseExitEvent.class));
      assertEquals(1, dispatcher.getListenersCount(RightMouseDownEvent.class));

      component.removeModifyListener(modifyListener);
      component.removeKeypressListener(keypressListener);
      component.removeFocusListener(focusListener);
      component.removeBlurListener(blurListener);
      component.removeMouseEnterListener(mouseEnterListener);
      component.removeMouseExitListener(mouseExitListener);
      component.removeRightMouseDownListener(rightMouseDownListener);

      assertEquals(0, dispatcher.getListenersCount(ModifyEvent.class));
      assertEquals(0, dispatcher.getListenersCount(KeypressEvent.class));
      assertEquals(0, dispatcher.getListenersCount(ToggleEvent.class));
      assertEquals(0, dispatcher.getListenersCount(FocusEvent.class));
      assertEquals(0, dispatcher.getListenersCount(BlurEvent.class));
      assertEquals(0, dispatcher.getListenersCount(MouseEnterEvent.class));
      assertEquals(0, dispatcher.getListenersCount(MouseExitEvent.class));
      assertEquals(0, dispatcher.getListenersCount(RightMouseDownEvent.class));
    }
  }

  @Nested
  @DisplayName("catchUp behavior")
  class CatchUp {

    void invokeCatchUp(AbstractFieldMock component)
        throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
      MethodUtils.invokeMethod(component, true, "catchUp");
    }

    @Test
    @DisplayName("calling twice should not be allowed")
    void callingTwiceShouldNotBeAllowed()
        throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
      AbstractFieldMock componentSpy = spy(component);
      invokeCatchUp(componentSpy);
      assertThrows(InvocationTargetException.class, () -> {
        invokeCatchUp(componentSpy);
      });
    }
  }
}
