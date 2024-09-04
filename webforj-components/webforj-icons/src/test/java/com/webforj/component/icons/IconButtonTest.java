package com.webforj.component.icons;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.element.Element;
import com.webforj.component.event.BlurEvent;
import com.webforj.component.event.FocusEvent;
import com.webforj.dispatcher.EventListener;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class IconButtonTest {

  IconButton component;

  @BeforeEach
  void setUp() {
    component = new IconButton("name", "pool");
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetEnabled() {
      component.setEnabled(true);
      assertTrue(component.isEnabled());

      assertEquals(false, component.getOriginalElement().getProperty("disabled"));

      component.setEnabled(false);
      assertTrue(!component.isEnabled());

      assertEquals(true, component.getOriginalElement().getProperty("disabled"));
    }

    @Test
    void shouldSetGetFocusableRingEnabled() {
      component.setFocusRingEnabled(true);
      assertTrue(component.isFocusRingEnabled());

      assertEquals(true, component.getOriginalElement().getProperty("focusVisible"));

      component.setFocusRingEnabled(false);
      assertTrue(!component.isFocusRingEnabled());

      assertEquals(false, component.getOriginalElement().getProperty("focusVisible"));
    }


    @Test
    void shouldSetGetFocusable() {
      component.setFocusable(true);
      assertTrue(component.isFocusable());

      assertEquals(1.0, component.getOriginalElement().getProperty("tabTraversable"));

      component.setFocusable(false);
      assertTrue(!component.isFocusable());

      assertEquals(0.0, component.getOriginalElement().getProperty("tabTraversable"));
    }

    @Test
    void shouldFocus() {
      IconButton spy = spy(component);
      Element element = spy(spy.getOriginalElement());
      when(spy.getOriginalElement()).thenReturn(element);

      spy.focus();
      verify(element).focus();
    }
  }

  @Nested
  class Events {

    @Test
    void shouldRegisterFocusListener() {
      IconButton spy = spy(component);
      Element element = spy(spy.getOriginalElement());
      when(spy.getOriginalElement()).thenReturn(element);

      EventListener<FocusEvent> listener = e -> {
      };

      spy.addFocusListener(listener);
      verify(element).addFocusListener(listener);
    }

    @Test
    void shouldRegisterBlurListener() {
      IconButton spy = spy(component);
      Element element = spy(spy.getOriginalElement());
      when(spy.getOriginalElement()).thenReturn(element);

      EventListener<BlurEvent> listener = e -> {
      };

      spy.addBlurListener(listener);
      verify(element).addBlurListener(listener);
    }
  }
}
