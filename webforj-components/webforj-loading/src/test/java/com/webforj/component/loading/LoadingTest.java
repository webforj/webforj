package com.webforj.component.loading;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;

import com.webforj.component.Component;
import com.webforj.component.element.PropertyDescriptorTester;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class LoadingTest {

  Loading component;

  @BeforeEach
  void setUp() {
    component = new Loading();
  }

  @Nested
  class Constructors {

    @Test
    void shouldCreateWithChildren() {
      Component content = mock(Component.class);
      component.add(content);
      assertTrue(component.getOriginalElement().hasComponent(content));
    }

    @Test
    void shouldCreateWithText() {
      component = new Loading("Hello, World!");
      assertEquals("Hello, World!", component.getText());
    }
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(Loading.class, component);
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
