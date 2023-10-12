package org.dwcj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.startup.type.BBjException;
import org.dwcj.component.ReflectionUtils;
import org.dwcj.component.event.ComponentEventListener;
import org.dwcj.component.event.KeypressEvent;
import org.dwcj.component.event.ModifyEvent;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class AbstractDwcFieldTest {

  @Mock
  BBjEditBox control;

  @InjectMocks
  AbstractDwcFieldMock component;

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
      ReflectionUtils.nullifyControl(component);
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
      ComponentEventListener<ModifyEvent> modifyListener = event -> {
      };
      ComponentEventListener<KeypressEvent> keypressListener = event -> {
      };

      component.onModify(modifyListener);
      component.onKeypress(keypressListener);
      assertEquals(1, component.getEventListeners(ModifyEvent.class).size());
      assertEquals(1, component.getEventListeners(KeypressEvent.class).size());

      component.removeModifyListener(modifyListener);
      component.removeKeypressListener(keypressListener);
      assertEquals(0, component.getEventListeners(ModifyEvent.class).size());
      assertEquals(0, component.getEventListeners(KeypressEvent.class).size());
    }
  }
}
