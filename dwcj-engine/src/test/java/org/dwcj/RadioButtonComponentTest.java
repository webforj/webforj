package org.dwcj;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.dwcj.component.radiobutton.RadioButton;
import org.dwcj.component.radiobutton.RadioButton.HorizontalTextPosition;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;

/**
 * Class to test the RadioButton component.
 */
class RadioButtonComponentTest {
  RadioButton radioButton;

  @BeforeEach
  void setUp() {
    radioButton = new RadioButton();
  }

  @Test
  @DisplayName("Id of RadioButton should be returned")
    void testRadioButtonId() {
    radioButton.setId("first");
    assertEquals("first", radioButton.getId());
  }

  @RepeatedTest(5)
  @DisplayName("Ensure correct handling of default value")
    void testDefaultRadioButtonId() {
    assertEquals("", radioButton.getId());
  }

  @Test
  @DisplayName("The RadioButton should be/not be checked")
    void testRadioButtonChecked() {
    radioButton.setChecked(true);
    assertEquals(true, radioButton.isChecked());
    radioButton.setChecked(false);
    assertEquals(false, radioButton.isChecked());
  }

  @RepeatedTest(5)
  @DisplayName("Ensure correct handling of default value")
    void testDefaultRadioButtonChecked() {
    assertEquals(false, radioButton.isChecked());
  }

  @Test
  @DisplayName("The RadioButton should be NOT editable")
    void testRadioButtonReadOnly() {
    radioButton.setReadOnly(true);
    assertEquals(true, radioButton.isReadOnly());
  }

  @Test
  @DisplayName("The RadioButton should be/ not be focused")
    void testRadioButtonFocusable() {
    radioButton.setFocusable(false);
    assertEquals(false, radioButton.isFocusable());
    radioButton.setFocusable(true);
    assertEquals(true, radioButton.isFocusable());
  }

  @RepeatedTest(5)
  @DisplayName("Ensure correct handling of default value")
    void testDefaultRadioButtonFocusable() {
    assertEquals(true, radioButton.isFocusable());
  }

  @Test
  @DisplayName("The RadioButton should be/not be tabTraversable")
    void testRadioButtonTabTraversable() {
    radioButton.setTabTraversable(false);
    assertEquals(false, radioButton.isTabTraversable());
    radioButton.setTabTraversable(true);
    assertEquals(true, radioButton.isTabTraversable());
  }

  @RepeatedTest(5)
  @DisplayName("Ensure correct handling of default value")
    void testDefaultRadioButtonTabTraversable() {
    assertEquals(true, radioButton.isTabTraversable());
  }

  @Test
  @DisplayName("The text of the RadioButton should be displayed")
    void testRadioButtonText() {
    radioButton.setText("myText");
    assertEquals("myText", radioButton.getText());
  }

  @RepeatedTest(5)
  @DisplayName("The default text of RadioButton should be handled")
    void testDefaultRadioButtonText() {
    assertEquals("", radioButton.getText());
  }

  @Test
  @DisplayName("The RadioButton should be/not be visible")
    void testRadioButtonVisible() {
    radioButton.setVisible(false);
    assertEquals(false, radioButton.isVisible());
    radioButton.setVisible(true);
    assertEquals(true, radioButton.isVisible());
  }

  @RepeatedTest(5)
  @DisplayName("Ensure correct handling of default value")
    void testDefaultRadioButtonVisible() {
    assertEquals(true, radioButton.isVisible());
  }

  @Test
  @DisplayName("The RadioButton should be/not be enabled")
    void testRadioButtonEnabled() {
    radioButton.setEnabled(false);
    assertEquals(false, radioButton.isEnabled());
    radioButton.setEnabled(true);
    assertEquals(true, radioButton.isEnabled());
  }

  @RepeatedTest(5)
  @DisplayName("Ensure correct handling of default value")
    void testDefaultRadioButtonEnabled() {
    assertEquals(true, radioButton.isEnabled());
  }

  @Test
  @DisplayName("The RadioButton should show a tooltip")
    void testRadioButtonToolTip() {
    radioButton.setTooltipText("myTool");
    assertEquals("myTool", radioButton.getTooltipText());
  }

  @RepeatedTest(5)
  @DisplayName("Ensure correct handling of default value")
    void testDefaultRadioButtonToolTip() {
    assertEquals("", radioButton.getTooltipText());
  }

  @Test
  @DisplayName("The RadioButton should have a userData")
    void testRadioButtonUserData() {
    radioButton.setUserData("myKey", "myValue");
    assertEquals("myValue", radioButton.getUserData("myKey"));
  }

  @RepeatedTest(5)
  @DisplayName("Ensure correct handling of empty value")
    void testDefaultRadioButtonUserData() {
    assertEquals(null, radioButton.getUserData(""));
  }

  @Test
  @DisplayName("The RadioButton should return the attribute")
    void testRadioButtonAttribute() {
    radioButton.setAttribute("disabled", "true");
    assertEquals(true, radioButton.isEnabled());
  }

  @RepeatedTest(5)
  @DisplayName("Ensure correct handling of empty value")
    void testDefaultRadioButtonAttribute() {
    assertEquals(null, radioButton.getAttribute(""));
  }

  @Test
  @DisplayName("The RadioButton should set/remove style")
    void testRadioButtonStyle() {
    radioButton.setStyle("background", "red");
    assertEquals("red", radioButton.getStyle("background"));
    radioButton.removeStyle("background");
    assertEquals(null, radioButton.getAttribute("background"));
  }

  @RepeatedTest(5)
  @DisplayName("Ensure correct handling of empty value")
    void testDefaultRadioButtonStyle() {
    assertEquals(null, radioButton.getAttribute(""));
  }

  @Test
  @DisplayName("Text of RadioButton should be displayed in respectively horizontal position")
    void testRadioButtonHorizontalText() {
    radioButton.setHorizontalTextPosition(HorizontalTextPosition.RIGHT);
    assertEquals(HorizontalTextPosition.RIGHT, radioButton.getHorizontalTextPosition());
    radioButton.setHorizontalTextPosition(HorizontalTextPosition.LEFT);
    assertEquals(HorizontalTextPosition.LEFT, radioButton.getHorizontalTextPosition());
    radioButton.setHorizontalTextPosition(HorizontalTextPosition.LEADING);
    assertEquals(HorizontalTextPosition.LEADING, radioButton.getHorizontalTextPosition());
    radioButton.setHorizontalTextPosition(HorizontalTextPosition.TRAILING);
    assertEquals(HorizontalTextPosition.TRAILING, radioButton.getHorizontalTextPosition());
    radioButton.setHorizontalTextPosition(HorizontalTextPosition.CENTER);
    assertEquals(HorizontalTextPosition.CENTER, radioButton.getHorizontalTextPosition());
  }

  @RepeatedTest(5)
  @DisplayName("Ensure handling of defaul value")
    void testDefaulRadioButtonHorizontalText() {
    assertEquals(HorizontalTextPosition.RIGHT, radioButton.getHorizontalTextPosition());
  }

  @Test
  @DisplayName("Event should be executed successfully")
    void testRadioButtonOnChange() {
    radioButton.onChange(e -> {
      radioButton.setText("Success");
    });
    assertEquals("Success", radioButton.getText());
    // cannot be tested yet
  }

  @Test
  @DisplayName("Checking if RadioButton is disabled")
    void testRadioButtonDisabled() {
    radioButton.setEnabled(false);
    assertEquals(true, radioButton.isDisabled());
  }

}