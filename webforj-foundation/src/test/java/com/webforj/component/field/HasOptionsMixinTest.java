package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjListSpinner;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class HasOptionsMixinTest {
  BBjListSpinner spinner;
  HasOptionsMixin hasOptionsMixin;

  @BeforeEach
  void setUp() {
    spinner = mock(BBjListSpinner.class);
    hasOptionsMixin = spy(new HasOptionsMixin(mock(DwcField.class)));
    when(hasOptionsMixin.inferListSpinner()).thenReturn(spinner);
  }

  @Nested
  class OptionsListApi {

    @Test
    void shouldSetGetOptionsWhenControlIsNull() {
      when(hasOptionsMixin.inferListSpinner()).thenReturn(null);

      List<String> options = Arrays.asList("Option1", "Option2", "Option3");
      hasOptionsMixin.setOptions(options);
      assertEquals(options, hasOptionsMixin.getOptions());
    }

    @Test
    void shouldSetGetOptionsWhenControlIsNotNull() throws BBjException {
      List<String> options = Arrays.asList("Option1", "Option2", "Option3");
      hasOptionsMixin.setOptions(options);

      verify(spinner).setSpinList(any(BBjVector.class));
      assertEquals(options, hasOptionsMixin.getOptions());
    }
  }

  @Nested
  class OptionIndexApi {

    @Test
    void shouldSetGetOptionIndexWhenControlIsNull() {
      when(hasOptionsMixin.inferListSpinner()).thenReturn(null);

      List<String> options = Arrays.asList("Option1", "Option2", "Option3");
      hasOptionsMixin.setOptions(options);
      hasOptionsMixin.setOptionIndex(1);
      assertEquals(1, hasOptionsMixin.getOptionIndex());
    }

    @Test
    void shouldSetGetOptionIndexWhenControlIsNotNull() throws BBjException {
      List<String> options = Arrays.asList("Option1", "Option2", "Option3");
      hasOptionsMixin.setOptions(options);

      hasOptionsMixin.setOptionIndex(1);

      verify(spinner).setListIndex(1);
    }
  }
}
