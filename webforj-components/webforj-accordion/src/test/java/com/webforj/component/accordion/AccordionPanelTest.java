package com.webforj.component.accordion;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;

import com.webforj.component.Component;
import com.webforj.component.accordion.event.AccordionPanelCloseEvent;
import com.webforj.component.accordion.event.AccordionPanelOpenEvent;
import com.webforj.component.accordion.event.AccordionPanelToggleEvent;
import com.webforj.component.element.PropertyDescriptorTester;
import com.webforj.dispatcher.EventListener;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class AccordionPanelTest {
  AccordionPanel component;

  @BeforeEach
  void setUp() {
    component = new AccordionPanel();
  }

  @Nested
  @DisplayName("Constructor")
  class ConstructorApi {

    @Test
    void shouldCreateWithLabel() {
      AccordionPanel panel = new AccordionPanel("Test Label");
      assertEquals("Test Label", panel.getText());
    }

    @Test
    void shouldCreateWithLabelAndContent() {
      Component content = mock(Component.class);
      AccordionPanel panel = new AccordionPanel("Test Label", content);
      assertEquals("Test Label", panel.getText());
    }
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(AccordionPanel.class, component, descriptor -> {
          return !Arrays.asList("opened", "disabled").contains(descriptor.getName());
        });
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }

    @Test
    void shouldOpenClosePanel() {
      component.open();
      assertEquals(true, component.getOriginalElement().getProperty("opened"));

      component.close();
      assertEquals(false, component.getOriginalElement().getProperty("opened"));
    }

    @Test
    void shouldSetGetEnabled() {
      assertTrue(component.isEnabled());

      component.setEnabled(false);
      assertEquals(true, component.getOriginalElement().getProperty("disabled"));

      component.setEnabled(true);
      assertEquals(false, component.getOriginalElement().getProperty("disabled"));
    }
  }

  @Nested
  @DisplayName("Slots API")
  class SlotsApi {

    @Test
    void shouldAddToHeader() {
      Component header = mock(Component.class);
      component.addToHeader(header);
      assertEquals(header, component.getOriginalElement().getFirstComponentInSlot("header"));
    }

    @Test
    void shouldSetAndGetIcon() {
      Component icon = mock(Component.class);
      component.setIcon(icon);
      assertEquals(icon, component.getIcon());
    }

    @Test
    void shouldReplaceExistingIcon() {
      Component icon1 = mock(Component.class);
      Component icon2 = mock(Component.class);
      component.setIcon(icon1);
      component.setIcon(icon2);
      assertEquals(icon2, component.getIcon());
    }

    @Test
    void shouldClearIconWithNull() {
      Component icon = mock(Component.class);
      component.setIcon(icon);
      component.setIcon(null);
      assertNull(component.getIcon());
    }

    @Test
    void shouldReturnNullForDefaultIcon() {
      assertNull(component.getIcon());
    }
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    void shouldAddToggleListener() {
      component.onToggle(event -> {
      });

      List<EventListener<AccordionPanelToggleEvent>> listeners =
          component.getEventListeners(AccordionPanelToggleEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<AccordionPanelToggleEvent>);
    }

    @Test
    void shouldAddOpenListener() {
      component.onOpen(event -> {
      });

      List<EventListener<AccordionPanelOpenEvent>> listeners =
          component.getEventListeners(AccordionPanelOpenEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<AccordionPanelOpenEvent>);
    }

    @Test
    void shouldAddCloseListener() {
      component.onClose(event -> {
      });

      List<EventListener<AccordionPanelCloseEvent>> listeners =
          component.getEventListeners(AccordionPanelCloseEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<AccordionPanelCloseEvent>);
    }
  }
}
