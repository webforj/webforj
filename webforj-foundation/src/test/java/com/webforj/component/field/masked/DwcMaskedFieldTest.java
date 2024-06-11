package com.webforj.component.field.masked;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjInput;
import com.basis.startup.type.BBjException;
import com.webforj.component.ReflectionUtils;
import com.webforj.concern.HasTypingMode.TypingMode;
import com.webforj.data.selection.SelectionRange;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class DwcMaskedFieldTest {

  @Mock
  BBjInput control;

  @InjectMocks
  DwcMaskedFieldMock component = new DwcMaskedFieldMock();

  @Nested
  class MaskApi {
    @Test
    void shouldSetMaskWhenControlIsNotNull() throws BBjException {
      String mask = "XX-XX-XX";
      component.setMask(mask);
      assertEquals(mask, component.getMask());

      verify(control).setMask(mask);
    }

    @Test
    void shouldSetMaskWhenControlIsNull() throws IllegalAccessException, BBjException {
      String mask = "XX-XX-XX";
      ReflectionUtils.nullifyControl(component);

      component.setMask(mask);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setMask(mask);
    }
  }

  @Nested
  class CaretPositionApi {
    @Test
    void shouldSetCaretPositionWhenControlIsNotNull() throws BBjException {
      int caretPosition = 5;

      when(control.getCaretPosition()).thenReturn(caretPosition);

      component.setCaretPosition(caretPosition);
      assertEquals(caretPosition, component.getCaretPosition());

      verify(control).setCaretPosition(caretPosition);
    }

    @Test
    void shouldSetCaretPositionWhenControlIsNull() throws IllegalAccessException, BBjException {
      int caretPosition = 5;
      ReflectionUtils.nullifyControl(component);

      component.setCaretPosition(caretPosition);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setCaretPosition(caretPosition);
    }
  }

  @Nested
  class TypingModeApi {

    @Test
    void shouldSetTypingModeWhenControlIsNotNull() throws BBjException {
      component.setTypingMode(TypingMode.INSERT);
      assertEquals(TypingMode.INSERT, component.getTypingMode());
      verify(control).setInsertMode(false);
    }

    @Test
    void shouldSetTypingModeWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      component.setTypingMode(TypingMode.INSERT);
      assertEquals(TypingMode.INSERT, component.getTypingMode());

      verify(control, times(0)).setInsertMode(false);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setInsertMode(false);
    }
  }

  @Nested
  class SelectionApi {

    @Test
    void shouldGetSelectedRangeWhenControlIsNotNull() {
      SelectionRange range = new SelectionRange(0, 2);

      component.setSelectionRange(range);
      assertEquals(range, component.getSelectionRange());

      verify(control).select(range.getStartOffset(), range.getEndOffset());
    }

    @Test
    void shouldGetSelectedRangeWhenControlIsNull() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      SelectionRange range = new SelectionRange(0, 2);
      component.setSelectionRange(range);
      assertEquals(range, component.getSelectionRange());

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).select(range.getStartOffset(), range.getEndOffset());
    }

    @Test
    void shouldGetSelectedText() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      SelectionRange range = new SelectionRange(0, 2);

      component.setSelectionRange(range);
      assertEquals("", component.getSelectedText());

      component.setText("123456");
      component.setSelectionRange(range);

      assertEquals("12", component.getSelectedText());
    }
  }

  @Nested
  class RestoreApi {

    @Test
    void shouldSetRestoreWhenControlIsNotNull() throws BBjException {
      String restoreValue = "restoreValue";
      component.setRestoreValue(restoreValue);
      assertEquals(restoreValue, component.getRestoreValue());

      verify(control).setRestore(restoreValue);
    }

    @Test
    void shouldSetRestoreWhenControlIsNull() throws IllegalAccessException, BBjException {
      String restoreValue = "restoreValue";
      ReflectionUtils.nullifyControl(component);

      component.setRestoreValue(restoreValue);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setRestore(restoreValue);
    }
  }
}
