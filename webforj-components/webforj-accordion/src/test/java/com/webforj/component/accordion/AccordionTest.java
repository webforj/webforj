package com.webforj.component.accordion;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import com.webforj.component.element.PropertyDescriptorTester;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class AccordionTest {
  Accordion component;

  @BeforeEach
  void setUp() {
    component = new Accordion();
  }

  @Nested
  @DisplayName("Constructor")
  class ConstructorApi {

    @Test
    void shouldCreateWithPanels() {
      AccordionPanel panelA = new AccordionPanel("A");
      AccordionPanel panelB = new AccordionPanel("B");
      Accordion accordion = new Accordion(panelA, panelB);

      assertEquals(2, accordion.getComponents().size());
    }
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(Accordion.class, component);
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }

    @Test
    void shouldSetGetMultiple() {
      assertFalse(component.isMultiple());

      component.setMultiple(true);
      assertTrue(component.isMultiple());

      component.setMultiple(false);
      assertFalse(component.isMultiple());
    }
  }

  @Nested
  @DisplayName("Methods API")
  class MethodsApi {

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldEnableOrDisableAllPanels(boolean enabled) {
      AccordionPanel panelA = new AccordionPanel("A");
      AccordionPanel panelB = new AccordionPanel("B");
      component.add(panelA, panelB);

      component.setEnabled(enabled);

      assertEquals(enabled, panelA.isEnabled());
      assertEquals(enabled, panelB.isEnabled());
    }

    @Test
    void shouldCloseAll() {
      AccordionPanel panelA = new AccordionPanel("A");
      AccordionPanel panelB = new AccordionPanel("B");

      component.add(panelA, panelB);
      panelA.open();
      panelB.open();

      component.closeAll();

      assertFalse(panelA.isOpened());
      assertFalse(panelB.isOpened());
    }

    @Test
    void shouldOpenAllWhenMultiple() {
      component.setMultiple(true);

      AccordionPanel panelA = new AccordionPanel("A");
      AccordionPanel panelB = new AccordionPanel("B");

      component.add(panelA, panelB);

      component.openAll();

      assertTrue(panelA.isOpened());
      assertTrue(panelB.isOpened());
    }
  }
}
