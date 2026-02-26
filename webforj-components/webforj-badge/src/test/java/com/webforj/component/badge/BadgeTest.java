package com.webforj.component.badge;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import com.webforj.component.element.PropertyDescriptorTester;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class BadgeTest {
  Badge component;

  @BeforeEach
  void setUp() {
    component = new Badge();
  }

  @Nested
  @DisplayName("Constructor")
  class ConstructorApi {

    @Test
    void shouldCreateEmpty() {
      Badge badge = new Badge();
      assertEquals("", badge.getLabel());
      assertEquals(BadgeTheme.DEFAULT, badge.getTheme());
      assertEquals(BadgeExpanse.SMALL, badge.getExpanse());
    }

    @Test
    void shouldCreateWithText() {
      Badge badge = new Badge("5");
      assertEquals("5", badge.getLabel());
      assertEquals(BadgeTheme.DEFAULT, badge.getTheme());
    }

    @Test
    void shouldCreateWithTextAndTheme() {
      Badge badge = new Badge("12", BadgeTheme.DANGER);
      assertEquals("12", badge.getLabel());
      assertEquals(BadgeTheme.DANGER, badge.getTheme());
    }

    @Test
    void shouldCreateWithTextAndComponents() {
      Badge icon = new Badge();
      Badge badge = new Badge("Done", icon);
      assertEquals("Done", badge.getLabel());
      assertEquals(1, badge.getComponents().size());
    }

    @Test
    void shouldCreateWithComponents() {
      Badge icon1 = new Badge();
      Badge icon2 = new Badge();
      Badge badge = new Badge(icon1, icon2);
      assertEquals("", badge.getLabel());
      assertEquals(2, badge.getComponents().size());
    }
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(Badge.class, component);
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }
  }
}
