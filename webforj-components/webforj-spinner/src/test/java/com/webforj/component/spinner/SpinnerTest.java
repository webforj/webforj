package com.webforj.component.spinner;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import com.webforj.component.Theme;
import com.webforj.component.element.PropertyDescriptorTester;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class SpinnerTest {

  Spinner component;

  @BeforeEach
  void setUp() {
    component = new Spinner();
  }

  @Nested
  class Constructors {

    @Test
    void shouldCreateSpinnerWithThemeAndExpanse() {
      Spinner s = new Spinner(Theme.GRAY, SpinnerExpanse.LARGE);

      assertEquals(Theme.GRAY, s.getTheme());
      assertEquals(SpinnerExpanse.LARGE, s.getExpanse());
    }

    @Test
    void shouldCreateSpinnerWithTheme() {
      Spinner s = new Spinner(Theme.GRAY);

      assertEquals(Theme.GRAY, s.getTheme());
      assertEquals(SpinnerExpanse.NONE, s.getExpanse());
    }

    @Test
    void shouldCreateSpinnerWithExpanse() {
      Spinner s = new Spinner(SpinnerExpanse.LARGE);

      assertEquals(Theme.DEFAULT, s.getTheme());
      assertEquals(SpinnerExpanse.LARGE, s.getExpanse());
    }

    @Test
    void shouldCreateSpinnerWithDefaultThemeAndExpanse() {
      Spinner s = new Spinner();

      assertEquals(Theme.DEFAULT, s.getTheme());
      assertEquals(SpinnerExpanse.NONE, s.getExpanse());
    }
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(Spinner.class, component);
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }
  }
}
