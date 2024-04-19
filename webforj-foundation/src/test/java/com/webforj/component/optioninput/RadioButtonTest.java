package com.webforj.component.optioninput;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjRadioButton;
import com.basis.startup.type.BBjException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class RadioButtonTest {

  @Mock
  BBjRadioButton control;

  @InjectMocks
  RadioButton component;

  @Nested
  class Constructors {

    @Test
    @DisplayName("Default constructor")
    void shouldConstruct() {
      assertNotNull(component);
    }

    @Test
    void shouldConstructWithNameTextAndChecked() {
      RadioButton button = new RadioButton("Name", "Text", true);
      assertNotNull(button);

      assertEquals("Name", button.getName());
      assertEquals("Text", button.getText());
      assertTrue(button.isChecked());
    }

    @Test
    void shouldConstructWithNameAndText() {
      RadioButton button = new RadioButton("Name", "Text");
      assertNotNull(button);

      assertEquals("Name", button.getName());
      assertEquals("Text", button.getText());
      assertFalse(button.isChecked());
    }

    @Test
    void shouldConstructWithTextAndChecked() {
      RadioButton button = new RadioButton("Text", true);
      assertNotNull(button);

      assertEquals("Text", button.getText());
      assertEquals("Text", button.getName());
      assertTrue(button.isChecked());
    }

    @Test
    void shouldConstructWithText() {
      RadioButton button = new RadioButton("Text");
      assertNotNull(button);

      assertEquals("Text", button.getText());
      assertEquals("Text", button.getName());
      assertFalse(button.isChecked());
    }

    // Switches

    @Test
    void shouldConstructSwitch() {
      RadioButton button = RadioButton.Switch();
      assertNotNull(button);

      assertEquals("", button.getName());
      assertEquals("", button.getText());
      assertFalse(button.isChecked());
      assertTrue(button.isSwitch());
    }

    @Test
    void shouldConstructSwitchWithNameTextAndChecked() {
      RadioButton button = RadioButton.Switch("Name", "Text", true);
      assertNotNull(button);

      assertEquals("Name", button.getName());
      assertEquals("Text", button.getText());
      assertTrue(button.isChecked());
      assertTrue(button.isSwitch());
    }

    @Test
    void shouldConstructSwitchWithNameAndText() {
      RadioButton button = RadioButton.Switch("Name", "Text");
      assertNotNull(button);

      assertEquals("Name", button.getName());
      assertEquals("Text", button.getText());
      assertFalse(button.isChecked());
      assertTrue(button.isSwitch());
    }

    @Test
    void shouldConstructSwitchWithTextAndChecked() {
      RadioButton button = RadioButton.Switch("Text", true);
      assertNotNull(button);

      assertEquals("Text", button.getText());
      assertEquals("Text", button.getName());
      assertTrue(button.isChecked());
      assertTrue(button.isSwitch());
    }

    @Test
    void shouldConstructSwitchWithText() {
      RadioButton button = RadioButton.Switch("Text");
      assertNotNull(button);

      assertEquals("Text", button.getText());
      assertEquals("Text", button.getName());
      assertFalse(button.isChecked());
      assertTrue(button.isSwitch());
    }
  }

  @ParameterizedTest
  @EnumSource(RadioButton.Activation.class)
  @DisplayName("Setting/getting activation")
  void settingGettingActivation(RadioButton.Activation activation) throws BBjException {
    assertNotNull(component);
    component.setActivation(activation);
    assertSame(component.getActivation(), activation);

    verify(control, times(1)).setProperty("activation", activation.getValue());
    verify(control, times(0)).getProperty("activation");
  }

  @Test
  @DisplayName("""
      isChecked returns false when the button belongs to a group
      and there is a button already checked
      """)
  void isCheckedWithGroup() {
    RadioButtonGroup group = new RadioButtonGroup(new RadioButton("Option 1", true),
        new RadioButton("Option 2", false), new RadioButton("Option 3", true));

    assertFalse(group.getButtons().get(0).isChecked());
    assertFalse(group.getButtons().get(1).isChecked());
    assertTrue(group.getButtons().get(2).isChecked());
  }

  @Test
  @DisplayName("setSwitch API")
  void setSwitchStyleApi() throws BBjException {
    component.setSwitch(true);
    assertTrue(component.isSwitch());

    verify(control, times(1)).setProperty("switch", true);
    verify(control, times(0)).getProperty("switch");
  }

  @Test
  @DisplayName("setSwitch Factory construction")
  void setSwitchStyleFactory() throws BBjException {
    RadioButton button = spy(RadioButton.Switch());
    assertTrue(button.isSwitch());
  }
}
