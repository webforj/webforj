package org.dwcj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
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
class TextFieldTest {

  @Mock
  BBjEditBox control;

  @InjectMocks
  TextField component = new TextField();

  @ParameterizedTest
  @EnumSource(TextField.Type.class)
  @DisplayName("setting/getting type when control is not null")
  void settingGettingTypeWhenControlIsNotNull(TextField.Type type) throws IllegalAccessException {
    component.setType(type);
    assertEquals(component.getType(), type);
  }

  @Test
  @DisplayName("placeholder")
  void placeholder() throws IllegalAccessException, BBjException {
    component.setPlaceholder("placeholder");
    assertEquals("placeholder", component.getPlaceholder());

    verify(control, times(1)).putClientProperty("placeholder", "placeholder");
  }
}
