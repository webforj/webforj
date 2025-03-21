package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.component.ReflectionUtils;
import com.webforj.data.selection.SelectionRange;
import com.webforj.exceptions.WebforjRuntimeException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class DwcTextFieldTest {

  @Mock
  BBjEditBox control;

  @InjectMocks
  DwcTextFieldMock component;

  @Test
  void shouldSetGetPattern() throws BBjException {
    String expectedPattern = "[0-9]{3}";
    component.setPattern(expectedPattern);
    assertEquals(expectedPattern, component.getPattern());

    verify(control, times(1)).setProperty("pattern", expectedPattern);
    verify(control, times(0)).getProperty("pattern");
  }

  @Nested
  @DisplayName("MaxLength API")
  class MaxLengthApi {

    @Test
    @DisplayName("setting/getting max length when control is null")
    void settingGettingMaxLengthWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);
      component.setMaxLength(10);
      assertEquals(10, component.getMaxLength());

      verify(control, times(0)).setProperty("maxlength", 10);
      verify(control, times(0)).getProperty("maxlength");
    }

    @Test
    @DisplayName("setting/getting max length when control is not null")
    void settingGettingMaxLengthWhenControlIsNotNull() throws BBjException {
      component.setMaxLength(10);
      assertEquals(10, component.getMaxLength());

      verify(control, times(1)).setProperty("maxlength", 10);
      verify(control, times(0)).getProperty("maxlength");
    }
  }

  @Nested
  @DisplayName("MinLength API")
  class MinLengthApi {

    @Test
    @DisplayName("setting/getting min length when control is null")
    void settingGettingMinLengthWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);
      component.setMinLength(10);
      assertEquals(10, component.getMinLength());

      verify(control, times(0)).setProperty("minlength", 10);
      verify(control, times(0)).getProperty("minlength");
    }

    @Test
    @DisplayName("setting/getting min length when control is not null")
    void settingGettingMinLengthWhenControlIsNotNull() throws BBjException {
      component.setMinLength(10);
      assertEquals(10, component.getMinLength());

      verify(control, times(1)).setProperty("minlength", 10);
      verify(control, times(0)).getProperty("minlength");
    }
  }

  @Nested
  @DisplayName("Value API")
  class SetValueApi {

    @Test
    @DisplayName("setting/getting value when control is null")
    void settingGettingValueWhenControlIsNull() throws IllegalAccessException {
      DwcTextFieldMock spy = spy(component);
      ReflectionUtils.nullifyControl(spy);
      spy.setValue("test");
      assertEquals("test", spy.getValue());

      verify(spy, times(1)).setText("test");
      verify(spy, times(1)).getText();
    }

    @Test
    @DisplayName("setting/getting value when control is not null")
    void settingGettingValueWhenControlIsNotNull() {
      DwcTextFieldMock spy = spy(component);
      doReturn("test").when(spy).getText();
      spy.setValue("test");
      assertEquals("test", spy.getValue());

      verify(spy, times(1)).setText("test");
      verify(spy, times(1)).getText();
    }
  }

  @Nested
  @DisplayName("Selection API")
  class SelectionApi {

    @Test
    @DisplayName("setting/getting selection when control is null")
    void settingGettingSelectionWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);
      component.setSelectionRange(0, 10);

      SelectionRange range = component.getSelectionRange();
      assertFalse(range.isEmpty());
      assertEquals(new SelectionRange(0, 10), range);

      verify(control, times(0)).select(0, 10);
      verify(control, times(0)).getSelection();
    }

    @Test
    @DisplayName("setting/getting selection when control is not null")
    void settingGettingSelectionWhenControlIsNotNull() throws BBjException {
      BBjVector selection = new BBjVector();
      selection.addItem(0);
      selection.addItem(0);
      selection.addItem(0);
      selection.addItem(10);
      doReturn(selection).when(control).getSelection();

      component.setSelectionRange(0, 10);

      SelectionRange range = component.getSelectionRange();

      assertFalse(range.isEmpty());
      assertEquals(new SelectionRange(0, 10), range);

      verify(control, times(1)).select(0, 10);
      verify(control, times(1)).getSelection();
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void reThrowDwcjRunTimeException() throws BBjException {
      doThrow(BBjException.class).when(control).getSelection();
      assertThrows(WebforjRuntimeException.class, () -> component.getSelectionRange());
    }

    @Test
    @DisplayName("onAttach will reapply the selection")
    void catchup() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      component.setSelectionRange(0, 10);

      verify(control, times(0)).select(0, 10);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).select(0, 10);
    }

    @Test
    @DisplayName("getSelectedText when the control is null")
    void getSelectedTextWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);
      verify(control, times(0)).getSelectedText();
    }

    @Test
    @DisplayName("getSelectedText when the control is not null")
    void getSelectedTextWhenControlIsNotNull() throws BBjException {
      component.getSelectedText();
      verify(control, times(1)).getSelectedText();
    }

    @Test
    @DisplayName("getSelectedText when the control throws BBjException a DwcjRuntimeException is thrown")
    void getSelectedTextWhenControlThrowsBBjException() throws BBjException {
      doThrow(BBjException.class).when(control).getSelectedText();
      assertThrows(WebforjRuntimeException.class, () -> component.getSelectedText());
    }

    @Test
    @DisplayName("getSelectedText when control is null but range is set")
    void getSelectedTextWhenControlIsNullButRangeIsSet() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      component.setText("The quick brown fox jumps over the lazy dog");
      component.setSelectionRange(0, 10);
      assertEquals("The quick ", component.getSelectedText());
    }

    @Test
    @DisplayName("getSelectedText when control is null but range is not set")
    void getSelectedTextWhenControlIsNullButRangeIsNotSet() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      component.setText("The quick brown fox jumps over the lazy dog");
      assertEquals("", component.getSelectedText());
    }
  }

  @Nested
  @DisplayName("AutoComplete API")
  class AutoCompleteApi {

    @Test
    @DisplayName("setting/getting autocomplete when control is null")
    void settingGettingAutoCompleteWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);
      component.setAutoComplete("on");
      assertEquals("on", component.getAutoComplete());

      verify(control, times(0)).setProperty("autocomplete", "on");
      verify(control, times(0)).getProperty("autocomplete");
    }

    @Test
    @DisplayName("setting/getting autocomplete when control is not null")
    void settingGettingAutoCompleteWhenControlIsNotNull() throws BBjException {
      component.setAutoComplete("on");
      assertEquals("on", component.getAutoComplete());

      verify(control, times(1)).setProperty("autocomplete", "on");
      verify(control, times(0)).getProperty("autocomplete");
    }
  }
}
