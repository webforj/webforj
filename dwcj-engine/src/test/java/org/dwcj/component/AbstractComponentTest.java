package org.dwcj.component;

import static org.junit.jupiter.api.Assertions.assertNotSame;

import org.dwcj.component.mocks.ComponentMock;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class AbstractComponentTest {
  ComponentMock component;

  @BeforeEach
  void setUp() {
    component = new ComponentMock();
  }

  @Test
  @DisplayName("Test getComponentId")
  void testGetComponent() {
    assertNotSame("", component.getComponentId());
  }
}
