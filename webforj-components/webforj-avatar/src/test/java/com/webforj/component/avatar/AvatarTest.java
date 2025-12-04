package com.webforj.component.avatar;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;

import com.webforj.component.Component;
import com.webforj.component.element.PropertyDescriptorTester;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class AvatarTest {

  Avatar component;

  @BeforeEach
  void setUp() {
    component = new Avatar();
  }

  @Nested
  @DisplayName("Constructors")
  class Constructors {

    @Test
    @DisplayName("should create empty avatar with default values")
    void shouldCreateEmptyAvatar() {
      Avatar avatar = new Avatar();

      assertEquals(Avatar.DEFAULT_LABEL, avatar.getLabel());
      assertEquals("A", avatar.getInitials());
      assertEquals(AvatarExpanse.MEDIUM, avatar.getExpanse());
      assertEquals(AvatarTheme.DEFAULT, avatar.getTheme());
      assertEquals(AvatarShape.CIRCLE, avatar.getShape());
    }

    @Test
    @DisplayName("should create avatar with label")
    void shouldCreateAvatarWithLabel() {
      Avatar avatar = new Avatar("John Doe");

      assertEquals("John Doe", avatar.getLabel());
      assertEquals("JD", avatar.getInitials());
    }

    @Test
    @DisplayName("should create avatar with label and initials")
    void shouldCreateAvatarWithLabelAndInitials() {
      Avatar avatar = new Avatar("John Doe", "JDA");

      assertEquals("John Doe", avatar.getLabel());
      assertEquals("JDA", avatar.getInitials());
    }

    @Test
    @DisplayName("should create avatar with label and components")
    void shouldCreateAvatarWithLabelAndComponents() {
      Component mockComponent = mock(Component.class);
      Avatar avatar = new Avatar("John Doe", mockComponent);

      assertEquals("John Doe", avatar.getLabel());
      assertEquals(1, avatar.getComponents().size());
    }

    @Test
    @DisplayName("should create avatar with components")
    void shouldCreateAvatarWithComponents() {
      Component mockComponent = mock(Component.class);
      Avatar avatar = new Avatar(mockComponent);

      assertEquals(Avatar.DEFAULT_LABEL, avatar.getLabel());
      assertEquals(1, avatar.getComponents().size());
    }
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(Avatar.class, component);
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }
  }
}
