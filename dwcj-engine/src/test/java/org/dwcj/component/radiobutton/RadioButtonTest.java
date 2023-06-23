package org.dwcj.component.radiobutton;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjRadioButton;
import com.basis.startup.type.BBjException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class RadioButtonTest {

  @Mock
  BBjRadioButton control;

  @InjectMocks
  RadioButton component;

  @ParameterizedTest
  @EnumSource(RadioButton.Activation.class)
  @DisplayName("Setting/getting activation")
  void settingGettingActivation(RadioButton.Activation activation) throws BBjException {
    assertNotNull(component);
    component.setActivation(activation);
    assertSame(component.getActivation(), activation);

    verify(control, times(1)).putClientProperty("activation", activation.getValue());
    verify(control, times(0)).getClientProperty("activation");
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
}
