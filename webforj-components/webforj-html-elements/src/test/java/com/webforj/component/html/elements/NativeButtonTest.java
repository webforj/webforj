package com.webforj.component.html.elements;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class NativeButtonTest {

  NativeButton component;

  @BeforeEach
  void setUp() {
    component = new NativeButton();
  }

  @Test
  void shouldSetGetDisableOnClick() {
    assertFalse(component.isDisableOnClick());

    assertSame(component, component.setDisableOnClick(true));
    assertTrue(component.isDisableOnClick());
    assertTrue(component.getElement().isDisableOnClick());

    component.setDisableOnClick(false);
    assertFalse(component.isDisableOnClick());
  }
}
