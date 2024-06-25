package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjSpinner;
import com.webforj.component.window.Window;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class SpinnableMixinTest {
  DwcField<?, ?> field;
  SpinnableMixin spinnableMixin;

  @BeforeEach
  void setUp() {
    field = mock(DwcField.class);
    spinnableMixin = spy(new SpinnableMixin(field));
  }

  @Test
  void shouldSpinUp() {
    BBjSpinner spinner = mock(BBjSpinner.class);
    when(spinnableMixin.inferSpinner()).thenReturn(spinner);

    spinnableMixin.spinUp();
    verify(spinner).spin(true);
  }

  @Test
  void shouldSpinDown() {
    BBjSpinner spinner = mock(BBjSpinner.class);
    when(spinnableMixin.inferSpinner()).thenReturn(spinner);

    spinnableMixin.spinDown();
    verify(spinner).spin(false);
  }

  @Test
  void shouldUpdateIndexWhenControlIsNull() {
    HasOptionsField mock = new HasOptionsField();
    spinnableMixin = new SpinnableMixin(mock);

    spinnableMixin.spinUp();
    assertEquals(1, mock.getOptionIndex());

    spinnableMixin.spinDown();
    assertEquals(0, mock.getOptionIndex());
  }

  class HasOptionsField extends DwcField<HasOptionsField, String>
      implements HasOptions<HasOptionsField> {
    private int optionIndex = 0;

    @Override
    public int getOptionIndex() {
      return optionIndex;
    }

    @Override
    public HasOptionsField setOptionIndex(int index) {
      optionIndex = index;
      return this;
    }

    @Override
    public HasOptionsField setOptions(List<String> list) {
      throw new UnsupportedOperationException("Unimplemented method 'setOptions'");
    }

    @Override
    public List<String> getOptions() {
      throw new UnsupportedOperationException("Unimplemented method 'getOptions'");
    }

    @Override
    protected String convertValue(String value) {
      throw new UnsupportedOperationException("Unimplemented method 'convertValue'");
    }

    @Override
    protected void onCreate(Window window) {
      throw new UnsupportedOperationException("Unimplemented method 'onCreate'");
    }
  }
}
