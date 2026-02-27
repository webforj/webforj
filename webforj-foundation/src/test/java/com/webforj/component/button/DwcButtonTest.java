package com.webforj.component.button;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjButton;
import com.basis.startup.type.BBjException;
import com.webforj.component.Component;
import com.webforj.component.ReflectionUtils;
import com.webforj.component.button.event.ButtonClickEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class DwcButtonTest {

  @Mock
  BBjButton control;

  @InjectMocks
  DwcButtonMock component;

  @Test
  void testSetGetName() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    assertEquals("", component.getName());

    component.setName("name");
    assertEquals("name", component.getName());
  }

  @Nested
  @DisplayName("DisableOnclick API")
  class DisableOnclickApi {
    @Test
    @DisplayName("disableOnClick when control is defined")
    void disableOnClickWhenControlIsDefined() throws BBjException {
      doReturn(true).when(control).getDisableOnClick();

      component.setDisableOnClick(true);
      assertTrue(component.isDisableOnClick());

      verify(control, times(1)).setDisableOnClick(true);
      verify(control, times(1)).getDisableOnClick();
    }

    @Test
    @DisplayName("disableOnClick when control is null")
    void disableOnClickWhenControlIsNull() throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.setDisableOnClick(true);
      assertTrue(component.isDisableOnClick());

      verify(control, times(0)).setDisableOnClick(true);
      verify(control, times(0)).getDisableOnClick();

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).setDisableOnClick(true);
    }

    @Test
    @DisplayName("""
            set/is disableOnClick re-throw DwcjRuntimeException when a BBjException is thrown
        """)
    void disableOnClickReThrowDwcjRuntimeExceptionWhenABbjExceptionIsThrown() throws BBjException {
      doThrow(BBjException.class).when(control).setDisableOnClick(true);
      doThrow(BBjException.class).when(control).getDisableOnClick();

      assertThrows(WebforjRuntimeException.class, () -> component.setDisableOnClick(true));
      assertThrows(WebforjRuntimeException.class, () -> component.isDisableOnClick());
    }
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    @DisplayName("adding/removing supported events")
    void addingRemovingSupportedEvents() {
      EventListener<ButtonClickEvent> clickListener = event -> {
      };

      ListenerRegistration<ButtonClickEvent> r = component.onClick(clickListener);
      assertEquals(1, component.getEventListeners(ButtonClickEvent.class).size());

      r.remove();
      assertEquals(0, component.getEventListeners(ButtonClickEvent.class).size());
    }
  }

  @Nested
  class SlotsApi {

    @Test
    void shouldSetAndGetPrefix() {
      Component prefixComponent = mock(Component.class);
      component.setPrefixComponent(prefixComponent);
      assertEquals(prefixComponent, component.getPrefixComponent());
    }

    @Test
    void shouldSetAndGetSuffix() {
      Component suffixComponent = mock(Component.class);
      component.setSuffixComponent(suffixComponent);
      assertEquals(suffixComponent, component.getSuffixComponent());
    }

    @Test
    void shouldSetAndGetIcon() {
      Component iconComponent = mock(Component.class);
      component.setIcon(iconComponent);
      assertEquals(iconComponent, component.getIcon());
    }

    @Test
    void shouldSetAndGetBadge() {
      Component badgeComponent = mock(Component.class);
      component.setBadge(badgeComponent);
      assertEquals(badgeComponent, component.getBadge());
    }

    @Test
    void shouldRemoveBadge() {
      Component badgeComponent = mock(Component.class);
      component.setBadge(badgeComponent);
      component.setBadge(null);
      assertNull(component.getBadge());
    }
  }
}
