package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjEditBoxSpinner;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.window.Window;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class TextFieldSpinnerTest {

  @Mock
  HasOptionsMixin mockMixin;

  @Mock
  SpinnableMixin mockSpinnableMixin;

  @InjectMocks
  TextFieldSpinner component = new TextFieldSpinner();

  @BeforeEach
  void setUp() {
    component = new TextFieldSpinner(mockMixin, mockSpinnableMixin);
  }

  @Test
  void shouldSetGetOptions() {
    List<String> options = Arrays.asList("Option1", "Option2", "Option3");
    component.setOptions(options);
    verify(mockMixin).setOptions(options);

    when(mockMixin.getOptions()).thenReturn(options);
    List<String> result = component.getOptions();
    assertEquals(options, result);
  }

  @Test
  void shouldSetGetOptionIndex() {
    int index = 1;
    component.setOptionIndex(index);
    verify(mockMixin).setOptionIndex(index);

    when(mockMixin.getOptionIndex()).thenReturn(index);
    int result = component.getOptionIndex();
    assertEquals(index, result);
  }

  @Test
  void shouldSpinUp() {
    component.spinUp();
    verify(mockSpinnableMixin).spinUp();
  }

  @Test
  void shouldSpinDown() {
    component.spinDown();
    verify(mockSpinnableMixin).spinDown();
  }

  @Test
  void shouldAttach() {
    component.onAttach();
    verify(mockMixin).onAttach();
  }

  @Test
  void shouldSendTextAgainOnAttachBecauseTheSpinnerSplitsItsCreationText()
      throws BBjException, IllegalAccessException {
    Window window = mock(Window.class);
    BBjWindow bbjWindow = mock(BBjWindow.class);
    BBjEditBoxSpinner spinner = mock(BBjEditBoxSpinner.class);
    WindowAccessor windowAccessor = mock(WindowAccessor.class);
    when(windowAccessor.getBBjWindow(window)).thenReturn(bbjWindow);
    when(bbjWindow.addEditBoxSpinner(anyInt(), anyString(), any(byte[].class))).thenReturn(spinner);
    component.setText("value");

    try (MockedStatic<WindowAccessor> accessor = mockStatic(WindowAccessor.class)) {
      accessor.when(WindowAccessor::getDefault).thenReturn(windowAccessor);
      component.onCreate(window);
      component.onAttach();
    }

    verify(bbjWindow).addEditBoxSpinner(anyInt(), eq("value"), any(byte[].class));
    verify(spinner, times(1)).setText("value");
  }
}
