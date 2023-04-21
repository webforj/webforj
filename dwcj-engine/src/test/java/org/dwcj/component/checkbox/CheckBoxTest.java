package org.dwcj.component.checkbox;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.dwcj.component.TextAlignable.Alignment;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class CheckBoxTest {
  CheckBox checkBox;

  @BeforeEach
  void setUp() {
    checkBox = new CheckBox();
  }

  @Test
  @DisplayName("isChecked should return a boolean")
  void testChecked() {
    Boolean returnedBoolean = checkBox.isChecked();
    assertNotNull(returnedBoolean, "Method failed");
    assertTrue(returnedBoolean instanceof Boolean, () -> "Method didnt return a boolean");
    checkBox.setChecked(!returnedBoolean);
    assertEquals(!returnedBoolean, checkBox.isChecked());
  }

  @Test
  @DisplayName("Test setText, getText")
  void testSetGetText() {
    String text = checkBox.getText();
    assertNotNull(text, "Method failed");
    assertEquals("", text, "Text returned nondefault value before being set");
    checkBox.setText("test");
    assertEquals("test", checkBox.getText(), "Setting new text was not successful");
  }

  @Test
  @DisplayName("Test setVisible, isVisible")
  void testSetGetVisible() {
    Boolean visible = checkBox.isVisible();
    assertNotNull(visible, "Method failed");
    assertEquals(true, visible, "Visible returned nondefault value before being set");
    checkBox.setVisible(false);
    assertEquals(false, checkBox.isVisible(), "Setting new visibility was not successful");
  }

  @Test
  @DisplayName("Test setEnabled, isEnabled")
  void testSetGetEnabled() {
    Boolean enabled = checkBox.isEnabled();
    assertNotNull(enabled, "Method failed");
    assertEquals(true, enabled, "enabled returned nondefault value before being set");
    checkBox.setEnabled(false);
    assertEquals(false, checkBox.isEnabled(), "Setting new enabled value was not successful");
  }

  @Test
  @DisplayName("Test setTooltip, getTooltip")
  void testSetGetTooltip() {
    String tooltip = checkBox.getTooltipText();
    assertNotNull(tooltip, "Method failed");
    assertEquals("", tooltip, "Tooltip returned nondefault value before being set");
    checkBox.setTooltipText("test");
    assertEquals("test", checkBox.getTooltipText(), "Setting new tooltip was not successful");
  }

  @Test
  @DisplayName("Test setId, getId")
  void testSetGetId() {
    String id = checkBox.getId();
    assertNotNull(id, "Method failed");
    assertEquals("", id, "Id returned nondefault value before being set");
    checkBox.setId("test");
    assertEquals("test", checkBox.getId(), "Setting new tooltip was not successful");
  }

  @Test
  @DisplayName("Test setStyle, getStyle")
  void testSetGetStyle() {
    assertNull(checkBox.getComputedStyle("testKey"));
    checkBox.setStyle("testKey", "testValue");
    assertEquals("testValue", checkBox.getComputedStyle("testKey"));
  }

  @Test
  @DisplayName("Test addClassName, removeClassName")
  void testClassNames() {
    assertNotNull(checkBox.addClassName("test"));
    assertNotNull(checkBox.removeClassName("test"));
  }

  @Test
  @DisplayName("Test isReadOnly, setReadOnly")
  void testSetGetReadOnly() {
    assertFalse(checkBox.isReadOnly());
    checkBox.setReadOnly(true);
    assertTrue(checkBox.isReadOnly());
    checkBox.setReadOnly(false);
    assertFalse(checkBox.isReadOnly());
  }

  @Test
  @DisplayName("Test isFocusable, setFocusable")
  void testSetGetFocusable() {
    assertTrue(checkBox.isFocusable());
    checkBox.setFocusable(false);
    assertFalse(checkBox.isFocusable());
    checkBox.setFocusable(true);
    assertTrue(checkBox.isFocusable());
  }

  @Test
  @DisplayName("Test isTabTraversable, setTabTraversable")
  void testSetGetTabTraversable() {
    assertTrue(checkBox.isTabTraversable());
    checkBox.setTabTraversable(false);
    assertFalse(checkBox.isTabTraversable());
    checkBox.setTabTraversable(true);
    assertTrue(checkBox.isTabTraversable());
  }

  @Test
  @DisplayName("Test getTextAlignment, setTextAlignment")
  void testSetGetTextAlignment() {
    assertEquals(Alignment.LEFT, checkBox.getTextAlignment());
    checkBox.setTextAlignment(Alignment.RIGHT);
    assertEquals(Alignment.RIGHT, checkBox.getTextAlignment());
  }

  @Test
  @DisplayName("Test getIndeterminate, setIndeterminate")
  void testSetGetIndeterminate() {
    assertFalse(checkBox.getIndeterminate());
    checkBox.setIndeterminate(true);
    assertTrue(checkBox.getIndeterminate());
  }

  @Test
  @DisplayName("Test getLabel, setLabel")
  void testSetGetLabel() {
    assertNull(checkBox.getLabel());
    checkBox.setLabel("test");
    assertEquals("test", checkBox.getLabel());
  }

  @Test
  @DisplayName("Test getName, setName")
  void testSetGetName() {
    assertNull(checkBox.getName());
    checkBox.setName("test");
    assertEquals("test", checkBox.getName());
  }

  @Test
  @DisplayName("Test getRequired, setRequired")
  void testSetGetRequired() {
    assertFalse(checkBox.getRequired());
    checkBox.setRequired(true);
    assertTrue(checkBox.getRequired());
  }
}
