package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.startup.type.BBjException;
import com.webforj.component.Expanse;
import com.webforj.component.ReflectionUtils;
import com.webforj.component.event.BlurEvent;
import com.webforj.component.event.KeypressEvent;
import com.webforj.component.event.ModifyEvent;
import com.webforj.data.concern.ValueChangeModeAware.ValueChangeMode;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class DwcFieldTest {

  @Mock
  BBjEditBox control;

  @InjectMocks
  DwcFieldMock component = new DwcFieldMock();

  @Nested
  class Constructs {

    @Test
    void shouldCreateFieldWithLabelValueAndPlaceholder() {
      component = new DwcFieldMock("label", "value", "placeholder");
      assertEquals("label", component.getLabel());
      assertEquals("value", component.getValue());
      assertEquals("placeholder", component.getPlaceholder());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelValueAndListener() {
      EventListener<ValueChangeEvent<String>> listener = event -> {
      };
      component = new DwcFieldMock("label", "value", listener);
      assertEquals("label", component.getLabel());
      assertEquals("value", component.getValue());
      assertEquals("", component.getPlaceholder());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabelAndValue() {
      component = new DwcFieldMock("label", "value");
      assertEquals("label", component.getLabel());
      assertEquals("value", component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelAndListener() {
      EventListener<ValueChangeEvent<String>> listener = event -> {
      };
      component = new DwcFieldMock("label", listener);
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithListener() {
      EventListener<ValueChangeEvent<String>> listener = event -> {
      };
      component = new DwcFieldMock(listener);
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabel() {
      component = new DwcFieldMock("label");
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithDefaults() {
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }
  }

  @Test
  @DisplayName("Label")
  void label() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.setLabel("label");
    assertEquals("label", component.getLabel());

    assertEquals("label", component.getProperty("label"));
  }

  @Test
  @DisplayName("Required")
  void required() throws BBjException {
    component.setRequired(true);
    assertEquals(true, component.isRequired());

    verify(control, times(1)).setProperty("required", true);
  }

  @Test
  @DisplayName("SpellCheck")
  void spellCheck() throws BBjException {
    component.setSpellCheck(true);
    assertEquals(true, component.isSpellCheck());

    verify(control, times(1)).setProperty("spellcheck", true);
  }

  @Nested
  @DisplayName("Focus API")
  class FocusApi {
    @Test
    @DisplayName("AutoFocus")
    void autoFocus() throws BBjException {
      component.setAutoFocus(true);
      assertEquals(true, component.isAutoFocus());

      verify(control, times(1)).setProperty("autofocus", true);
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

      ListenerRegistration<ModifyEvent> r1 = component.onModify(modifyListener);
      ListenerRegistration<KeypressEvent> r2 = component.onKeypress(keypressListener);
      assertEquals(1, component.getEventListeners(ModifyEvent.class).size());
      assertEquals(1, component.getEventListeners(KeypressEvent.class).size());

      r1.remove();
      r2.remove();
      assertEquals(0, component.getEventListeners(ModifyEvent.class).size());
      assertEquals(0, component.getEventListeners(KeypressEvent.class).size());
    }

    @Test
    void shouldConfigureValueChangeEvent() {
      component.onValueChange(e -> {
      });
      assertEquals(1, component.getEventListeners(ModifyEvent.class).size());
      assertEquals(0, component.getEventListeners(BlurEvent.class).size());

      component.setValueChangeMode(ValueChangeMode.ON_BLUR);
      component.onValueChange(e -> {
      });
      assertEquals(1, component.getEventListeners(ModifyEvent.class).size());
      assertEquals(1, component.getEventListeners(BlurEvent.class).size());
    }
  }
}
