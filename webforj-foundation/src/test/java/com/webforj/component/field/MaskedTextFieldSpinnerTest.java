package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class MaskedTextFieldSpinnerTest {

  @Mock
  HasOptionsMixin mockMixin;

  @Mock
  SpinnableMixin mockSpinnableMixin;

  @InjectMocks
  MaskedTextFieldSpinner component = new MaskedTextFieldSpinner();

  @BeforeEach
  void setUp() {
    component = new MaskedTextFieldSpinner(mockMixin, mockSpinnableMixin);
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
}
