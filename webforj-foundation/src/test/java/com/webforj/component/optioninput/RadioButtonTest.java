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
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
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
    void shouldCreateRadioButtonWithTextCheckedAndListener() {
      String text = "Test RadioButton";
      boolean checked = true;
      EventListener<ValueChangeEvent<Boolean>> listener = event -> {
        // Event listener implementation
      };

      RadioButton radioButton = RadioButton.Switch(text, checked, listener);

      assertEquals(text, radioButton.getText());
      assertTrue(radioButton.isChecked());

      // @see https://github.com/webforj/webforj/issues/643
      // assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateRadioButtonWithTextAndChecked() {
      String text = "Test RadioButton";
      boolean checked = true;

      RadioButton radioButton = RadioButton.Switch(text, checked);

      assertEquals(text, radioButton.getText());
      assertTrue(radioButton.isChecked());
    }

    @Test
    void shouldCreateRadioButtonWithText() {
      String text = "Test RadioButton";

      RadioButton radioButton = RadioButton.Switch(text);

      assertEquals(text, radioButton.getText());
      assertFalse(radioButton.isChecked());
    }

    @Test
    void shouldCreateRadioButtonWithChecked() {
      boolean checked = true;

      RadioButton radioButton = RadioButton.Switch(checked);

      assertEquals("", radioButton.getText());
      assertTrue(radioButton.isChecked());
    }

    @Test
    void shouldCreateRadioButtonWithNameTextCheckedAndListener() {
      String name = "radio1";
      String text = "Test RadioButton";
      boolean checked = true;
      EventListener<ValueChangeEvent<Boolean>> listener = event -> {
        // Event listener implementation
      };

      RadioButton radioButton = RadioButton.Switch(name, text, checked, listener);

      assertEquals(name, radioButton.getName());
      assertEquals(text, radioButton.getText());
      assertTrue(radioButton.isChecked());
    }

    @Test
    void shouldCreateRadioButtonWithNameTextAndChecked() {
      String name = "radio1";
      String text = "Test RadioButton";
      boolean checked = true;

      RadioButton radioButton = RadioButton.Switch(name, text, checked);

      assertEquals(name, radioButton.getName());
      assertEquals(text, radioButton.getText());
      assertTrue(radioButton.isChecked());
    }

    @Test
    void shouldCreateRadioButtonWithNameTextAndListener() {
      String name = "radio1";
      String text = "Test RadioButton";
      EventListener<ValueChangeEvent<Boolean>> listener = event -> {
        // Event listener implementation
      };

      RadioButton radioButton = RadioButton.Switch(name, text, listener);

      assertEquals(name, radioButton.getName());
      assertEquals(text, radioButton.getText());
      assertFalse(radioButton.isChecked());
    }

    @Test
    void shouldCreateRadioButtonWithNameAndText() {
      String name = "radio1";
      String text = "Test RadioButton";

      RadioButton radioButton = RadioButton.Switch(name, text);

      assertEquals(name, radioButton.getName());
      assertEquals(text, radioButton.getText());
      assertFalse(radioButton.isChecked());
    }

    @Test
    void shouldCreateRadioButtonWithNoParameters() {
      RadioButton radioButton = RadioButton.Switch();

      assertEquals("", radioButton.getText());
      assertFalse(radioButton.isChecked());
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
