package com.webforj.component.toast;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import com.webforj.component.element.PropertyDescriptorTester;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class ToastTest {

  Toast component;

  @BeforeEach
  void setUp() {
    component = new Toast();
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(Toast.class, component);
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }

    @Test
    void shouldRemoveHtmlWhenSetGetTextUsed() {
      component.setText("<div>Hello, World!</div>");
      assertEquals("Hello, World!", component.getText());

      component.setHtml("<div>Hello, World!</div>");
      assertEquals("<div>Hello, World!</div>", component.getHtml());
      assertEquals("Hello, World!", component.getText());
    }
  }
}
