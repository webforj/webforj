package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import org.junit.jupiter.api.DisplayName;
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
}
