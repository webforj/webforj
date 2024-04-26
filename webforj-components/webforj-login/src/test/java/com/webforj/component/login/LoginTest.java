package com.webforj.component.login;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;

import com.webforj.component.Component;
import com.webforj.component.element.PropertyDescriptorTester;
import com.webforj.component.login.event.LoginCancelEvent;
import com.webforj.component.login.event.LoginSubmitEvent;
import com.webforj.dispatcher.EventListener;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class LoginTest {

  Login component;

  @BeforeEach
  void setUp() {
    component = new Login();
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(Login.class, component, descriptor -> {
          return !Arrays.asList("opened", "disabled").contains(descriptor.getName());
        });
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }

    @Test
    void shouldShowHideLogin() {
      component.open();

      assertEquals(true, component.isOpened());
      assertEquals(true, component.getOriginalElement().getProperty("opened"));

      component.close();

      assertEquals(false, component.isOpened());
      assertEquals(false, component.getOriginalElement().getProperty("opened"));
    }

    @Test
    void shouldEnableDisableLogin() {
      component.setEnabled(true);

      assertEquals(true, component.isEnabled());
      assertEquals(false, component.getOriginalElement().getProperty("disabled"));

      component.setEnabled(false);

      assertEquals(false, component.isEnabled());
      assertEquals(true, component.getOriginalElement().getProperty("disabled"));
    }
  }

  @Nested
  @DisplayName("Slots API")
  class SlotsApi {

    @Test
    void shouldAddToBeforeHeader() {
      Component header = mock(Component.class);
      component.addToBeforeHeader(header);
      assertEquals(header, component.getOriginalElement().getFirstComponentInSlot("before-header"));
    }

    @Test
    void shouldAddToAfterHeader() {
      Component header = mock(Component.class);
      component.addToAfterHeader(header);
      assertEquals(header, component.getOriginalElement().getFirstComponentInSlot("after-header"));
    }

    @Test
    void shouldAddToBeforeContent() {
      Component header = mock(Component.class);
      component.addToBeforeContent(header);
      assertEquals(header,
          component.getOriginalElement().getFirstComponentInSlot("before-content"));
    }

    @Test
    void shouldAddToAfterContent() {
      Component header = mock(Component.class);
      component.addToAfterContent(header);
      assertEquals(header, component.getOriginalElement().getFirstComponentInSlot("after-content"));
    }

    @Test
    void shouldAddToBeforeForm() {
      Component header = mock(Component.class);
      component.addToBeforeForm(header);
      assertEquals(header, component.getOriginalElement().getFirstComponentInSlot("before-form"));
    }

    @Test
    void shouldAddToAfterForm() {
      Component header = mock(Component.class);
      component.addToAfterForm(header);
      assertEquals(header, component.getOriginalElement().getFirstComponentInSlot("after-form"));
    }

    @Test
    void shouldAddToBeforeFooter() {
      Component header = mock(Component.class);
      component.addToBeforeFooter(header);
      assertEquals(header, component.getOriginalElement().getFirstComponentInSlot("before-footer"));
    }

    @Test
    void shouldAddToAfterFooter() {
      Component header = mock(Component.class);
      component.addToAfterFooter(header);
      assertEquals(header, component.getOriginalElement().getFirstComponentInSlot("after-footer"));
    }
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    void shouldAddSubmitListener() {
      component.onSubmit(event -> {
      });

      List<EventListener<LoginSubmitEvent>> listeners =
          component.getEventListeners(LoginSubmitEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<LoginSubmitEvent>);
    }

    @Test
    void shouldReceiveSubmitPayload() {
      Map<String, Object> eventMap = new HashMap<>();
      eventMap.put("username", "testuser");
      eventMap.put("password", "testpass");
      eventMap.put("rememberme", false);

      LoginSubmitEvent mockEvent = new LoginSubmitEvent(component, eventMap);
      AtomicReference<String> capturedUsername = new AtomicReference<>();
      AtomicReference<String> capturedPassword = new AtomicReference<>();

      component.addSubmitListener(event -> {
        capturedUsername.set(event.getUsername());
        capturedPassword.set(event.getPassword());
      });

      component.getEventListeners(LoginSubmitEvent.class).get(0).onEvent(mockEvent);

      assertEquals("testuser", capturedUsername.get());
      assertEquals("testpass", capturedPassword.get());
      assertEquals(false, mockEvent.isRememberMe());
    }

    @Test
    void shouldAddCancelListener() {
      component.onCancel(event -> {
      });

      List<EventListener<LoginCancelEvent>> listeners =
          component.getEventListeners(LoginCancelEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<LoginCancelEvent>);
    }
  }
}
