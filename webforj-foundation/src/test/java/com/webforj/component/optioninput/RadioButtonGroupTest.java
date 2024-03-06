package com.webforj.component.optioninput;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjRadioGroup;
import com.basis.startup.type.BBjException;
import com.webforj.component.window.Window;
import com.webforj.exceptions.WebforjRuntimeException;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class RadioButtonGroupTest {

  @Mock
  BBjRadioGroup group;

  @Mock
  Window window;

  @InjectMocks
  RadioButtonGroup component = new RadioButtonGroup();

  void nullifyGroup() throws IllegalAccessException {
    FieldUtils.writeField(component, "group", null, true);
  }

  @Nested
  @DisplayName("Adding API")
  class AddingApi {

    @Test
    @DisplayName("When group is null")
    void whenGroupIsNull() throws BBjException, IllegalAccessException {
      nullifyGroup();
      RadioButton[] buttons = {new RadioButton(), new RadioButton()};
      component.add(buttons);

      verify(window, times(0)).add(any(RadioButton.class));
      assertEquals(2, component.getRadioButtons().size());
    }

    @Test
    @DisplayName("When group is not null")
    void whenGroupIsNotNull() throws BBjException, IllegalAccessException {
      RadioButton[] buttons = {new RadioButton(), new RadioButton()};
      component.add(buttons);

      verify(window, times(2)).add(any(RadioButton.class));
      verify(group, times(2)).add(any());
    }

    @Test
    @DisplayName("When button is already attached to a window")
    void whenButtonIsAlreadyAttachedToAWindow() throws BBjException, IllegalAccessException {
      RadioButton[] buttons = {spy(new RadioButton()), new RadioButton()};
      when(buttons[0].isAttached()).thenReturn(true);

      component.add(buttons);

      verify(window, times(0)).add(buttons[0]);
      verify(window, times(1)).add(buttons[1]);
      verify(group, times(2)).add(any());
    }

    @Test
    @DisplayName("Buttons are assigned to the group")
    void buttonsAreAssignedToTheGroup() throws BBjException, IllegalAccessException {
      RadioButton[] buttons = {new RadioButton(), new RadioButton()};
      component.add(buttons);

      assertEquals(component, buttons[0].getButtonGroup());
      assertEquals(component, buttons[1].getButtonGroup());
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void whenControlThrowsBBjExceptionADwcjRuntimeExceptionIsThrown()
        throws BBjException, IllegalAccessException {
      RadioButton[] buttons = {new RadioButton(), new RadioButton()};
      doThrow(BBjException.class).when(group).add(any());

      assertThrows(WebforjRuntimeException.class, () -> component.add(buttons));
    }
  }

  @Nested
  @DisplayName("Removing API")
  class RemovingApi {

    @Test
    @DisplayName("When group is null")
    void whenGroupIsNull() throws BBjException, IllegalAccessException {
      nullifyGroup();
      RadioButton[] buttons = {new RadioButton(), new RadioButton()};
      component.add(buttons);

      component.remove(buttons[1]);

      assertEquals(1, component.getRadioButtons().size());
      assertEquals(buttons[0], component.getRadioButtons().get(0));
    }

    @Test
    @DisplayName("When group is not null")
    void whenGroupIsNotNull() throws BBjException, IllegalAccessException {
      RadioButton[] buttons = {new RadioButton(), new RadioButton()};
      component.add(buttons);

      component.remove(buttons[1]);

      assertEquals(1, component.getRadioButtons().size());
      assertEquals(buttons[0], component.getRadioButtons().get(0));

      verify(group, times(1)).remove(any());
    }

    @Test
    @DisplayName("Buttons are de-assigned from the group")
    void buttonsAreAssignedToTheGroup() throws BBjException, IllegalAccessException {
      RadioButton[] buttons = {new RadioButton(), new RadioButton()};
      component.add(buttons);

      component.remove(buttons);

      assertNull(buttons[0].getButtonGroup());
      assertNull(buttons[1].getButtonGroup());
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void whenControlThrowsBBjExceptionADwcjRuntimeExceptionIsThrown()
        throws BBjException, IllegalAccessException {
      RadioButton[] buttons = {new RadioButton(), new RadioButton()};
      doThrow(BBjException.class).when(group).remove(any());

      assertThrows(WebforjRuntimeException.class, () -> component.remove(buttons[1]));
    }
  }

  @Nested
  @DisplayName("getChecked API")
  class GetCheckedApi {

    @Test
    @DisplayName("When group is null")
    void whenGroupIsNull() throws BBjException, IllegalAccessException {
      nullifyGroup();
      RadioButton[] buttons =
          {new RadioButton("Option 1", false), new RadioButton("Options 2", true)};
      component.add(buttons);

      assertEquals(buttons[1], component.getChecked());
    }

    @Test
    @DisplayName("When group is not null")
    void whenGroupIsNotNull() throws BBjException, IllegalAccessException {
      RadioButton[] buttons =
          {new RadioButton("Option 1", false), new RadioButton("Options 2", true)};
      component.add(buttons);

      RadioButton checked = component.getChecked();
      verify(group, times(1)).getSelected();
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void whenControlThrowsBBjExceptionADwcjRuntimeExceptionIsThrown()
        throws BBjException, IllegalAccessException {
      RadioButton[] buttons =
          {new RadioButton("Option 1", false), new RadioButton("Options 2", true)};
      doThrow(BBjException.class).when(group).getSelected();

      assertThrows(WebforjRuntimeException.class, () -> component.getChecked());
    }
  }

  @Nested
  @DisplayName("Name API")
  class NameApi {

    @Test
    @DisplayName("When group is null")
    void whenGroupIsNull() throws BBjException, IllegalAccessException {
      nullifyGroup();
      component.setName("name");

      assertEquals("name", component.getName());
      verify(group, times(0)).setName(any());
    }

    @Test
    @DisplayName("When group is not null")
    void whenGroupIsNotNull() throws BBjException, IllegalAccessException {
      component.setName("name");

      assertEquals("name", component.getName());
      verify(group, times(1)).setName(any());
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void whenControlThrowsBBjExceptionADwcjRuntimeExceptionIsThrown()
        throws BBjException, IllegalAccessException {
      doThrow(BBjException.class).when(group).setName(any());
      assertThrows(WebforjRuntimeException.class, () -> component.setName("name"));
    }
  }
}
