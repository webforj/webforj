package com.webforj.component.optioninput;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import com.basis.bbj.proxies.sysgui.BBjRadioGroup;
import com.basis.startup.type.BBjException;
import com.webforj.component.Component;
import com.webforj.component.ComponentRegistry;
import com.webforj.component.window.Window;
import com.webforj.concern.HasClientValidationStyle.ValidationStyle;
import com.webforj.concern.HasComponents;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.List;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.junit.jupiter.api.BeforeEach;
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
  BBjRadioGroup bbjGroup;

  @Mock
  Window window;

  @InjectMocks
  RadioButtonGroup component = new RadioButtonGroup();

  void nullifyGroup() throws IllegalAccessException {
    FieldUtils.writeField(component, "group", null, true);
  }

  @Nested
  class Constructors {

    @Test
    void shouldCreateRadioButtonGroupWithNameButtonsAndListener() {
      String name = "group1";
      RadioButton button1 = new RadioButton("Option 1");
      RadioButton button2 = new RadioButton("Option 2");
      EventListener<ValueChangeEvent<String>> listener = event -> {
        // Event listener implementation
      };

      RadioButtonGroup group = new RadioButtonGroup(name, List.of(button1, button2), listener);

      assertEquals(name, group.getName());
      assertEquals(2, group.getRadioButtons().size());
    }

    @Test
    void shouldCreateRadioButtonGroupWithNameAndButtons() {
      String name = "group1";
      RadioButton button1 = new RadioButton("Option 1");
      RadioButton button2 = new RadioButton("Option 2");

      RadioButtonGroup group = new RadioButtonGroup(name, List.of(button1, button2));

      assertEquals(name, group.getName());
      assertEquals(2, group.getRadioButtons().size());
    }

    @Test
    void shouldCreateRadioButtonGroupWithNameAndVarArgsButtons() {
      String name = "group1";
      RadioButton button1 = new RadioButton("Option 1");
      RadioButton button2 = new RadioButton("Option 2");

      RadioButtonGroup group = new RadioButtonGroup(name, button1, button2);

      assertEquals(name, group.getName());
      assertEquals(2, group.getRadioButtons().size());
    }

    @Test
    void shouldCreateRadioButtonGroupWithName() {
      String name = "group1";

      RadioButtonGroup group = new RadioButtonGroup(name);

      assertEquals(name, group.getName());
      assertTrue(group.getRadioButtons().isEmpty());
    }

    @Test
    void shouldCreateRadioButtonGroupWithVarArgsButtons() {
      RadioButton button1 = new RadioButton("Option 1");
      RadioButton button2 = new RadioButton("Option 2");

      RadioButtonGroup group = new RadioButtonGroup(button1, button2);

      assertEquals(2, group.getRadioButtons().size());
    }

    @Test
    void shouldCreateRadioButtonGroupWithNoParameters() {
      RadioButtonGroup group = new RadioButtonGroup();

      assertTrue(group.getRadioButtons().isEmpty());
    }
  }

  @Nested
  @DisplayName("Adding API")
  class AddingApi {

    @Test
    @DisplayName("When group is null")
    void whenGroupIsNull() throws IllegalAccessException {
      nullifyGroup();
      RadioButton[] buttons = {new RadioButton(), new RadioButton()};
      component.add(buttons);

      verify(window, times(0)).add(any(RadioButton.class));
      assertEquals(2, component.getRadioButtons().size());
    }

    @Test
    @DisplayName("When group is not null")
    void whenGroupIsNotNull() throws BBjException {
      RadioButton[] buttons = {spy(new RadioButton()), spy(new RadioButton())};
      // Mock buttons to become attached after being added to window
      when(buttons[0].isAttached()).thenReturn(false, true);
      when(buttons[1].isAttached()).thenReturn(false, true);

      component.add(buttons);

      verify(window, times(2)).add(any(RadioButton.class));
      verify(bbjGroup, times(2)).add(any());
    }

    @Test
    @DisplayName("When button is already attached to a window")
    void whenButtonIsAlreadyAttachedToAWindow() throws BBjException {
      RadioButton[] buttons = {spy(new RadioButton()), spy(new RadioButton())};
      // First button is always attached, second becomes attached after add
      when(buttons[0].isAttached()).thenReturn(true);
      when(buttons[1].isAttached()).thenReturn(false, true);

      component.add(buttons);

      verify(window, times(0)).add(buttons[0]);
      verify(window, times(1)).add(buttons[1]);
      verify(bbjGroup, times(2)).add(any());
    }

    @Test
    @DisplayName("Buttons are assigned to the group")
    void buttonsAreAssignedToTheGroup() {
      RadioButton[] buttons = {new RadioButton(), new RadioButton()};
      component.add(buttons);

      assertEquals(component, buttons[0].getButtonGroup());
      assertEquals(component, buttons[1].getButtonGroup());
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void whenControlThrowsBBjExceptionADwcjRuntimeExceptionIsThrown() throws BBjException {
      RadioButton[] buttons = {spy(new RadioButton()), spy(new RadioButton())};
      // Mock buttons as attached so BBjRadioGroup.add is called synchronously
      when(buttons[0].isAttached()).thenReturn(false, true);
      doThrow(BBjException.class).when(bbjGroup).add(any());

      assertThrows(WebforjRuntimeException.class, () -> component.add(buttons));
    }

  }

  @Nested
  @DisplayName("Parent Container API")
  class ParentContainerApi {

    @Test
    @DisplayName("When parent implements HasComponents, buttons are added to parent not window")
    void shouldAddButtonsToParentContainerInsteadOfWindow() throws BBjException {
      Component mockParent =
          mock(Component.class, withSettings().extraInterfaces(HasComponents.class));
      when(mockParent.isAttached()).thenReturn(false);

      // ComponentRegistry should set the parent
      ComponentRegistry registry = new ComponentRegistry(mockParent, c -> {
      });
      registry.add(component);

      // buttons go to parent (HasComponents), not window
      RadioButton[] buttons = {new RadioButton(), new RadioButton()};
      component.add(buttons);

      // buttons were added to parent container
      verify((HasComponents) mockParent, times(2)).add(any(RadioButton.class));
      verify(window, times(0)).add(any(RadioButton.class));
    }
  }

  @Nested
  @DisplayName("Removing API")
  class RemovingApi {

    @Test
    @DisplayName("When group is null")
    void whenGroupIsNull() throws IllegalAccessException {
      nullifyGroup();
      RadioButton[] buttons = {new RadioButton(), new RadioButton()};
      component.add(buttons);

      component.remove(buttons[1]);

      assertEquals(1, component.getRadioButtons().size());
      assertEquals(buttons[0], component.getRadioButtons().get(0));
    }

    @Test
    @DisplayName("When group is not null")
    void whenGroupIsNotNull() throws BBjException {
      RadioButton[] buttons = {new RadioButton(), new RadioButton()};
      component.add(buttons);

      component.remove(buttons[1]);

      assertEquals(1, component.getRadioButtons().size());
      assertEquals(buttons[0], component.getRadioButtons().get(0));

      verify(bbjGroup, times(1)).remove(any());
    }

    @Test
    @DisplayName("Buttons are de-assigned from the group")
    void buttonsAreAssignedToTheGroup() {
      RadioButton[] buttons = {new RadioButton(), new RadioButton()};
      component.add(buttons);

      component.remove(buttons);

      assertNull(buttons[0].getButtonGroup());
      assertNull(buttons[1].getButtonGroup());
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void whenControlThrowsBBjExceptionADwcjRuntimeExceptionIsThrown() throws BBjException {
      RadioButton[] buttons = {new RadioButton(), new RadioButton()};
      doThrow(BBjException.class).when(bbjGroup).remove(any());

      assertThrows(WebforjRuntimeException.class, () -> component.remove(buttons[1]));
    }
  }

  @Nested
  @DisplayName("getChecked API")
  class GetCheckedApi {

    @Test
    @DisplayName("When group is null")
    void whenGroupIsNull() throws IllegalAccessException {
      nullifyGroup();
      RadioButton[] buttons =
          {new RadioButton("Option 1", false), new RadioButton("Options 2", true)};
      component.add(buttons);

      assertEquals(buttons[1], component.getChecked());
    }

    @Test
    @DisplayName("When group is not null")
    void whenGroupIsNotNull() throws BBjException {
      RadioButton[] buttons =
          {new RadioButton("Option 1", false), new RadioButton("Options 2", true)};
      component.add(buttons);

      component.getChecked();
      verify(bbjGroup, times(1)).getSelected();
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void whenControlThrowsBBjExceptionADwcjRuntimeExceptionIsThrown() throws BBjException {
      doThrow(BBjException.class).when(bbjGroup).getSelected();

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
      verify(bbjGroup, times(0)).setName(any());
    }

    @Test
    @DisplayName("When group is not null")
    void whenGroupIsNotNull() throws BBjException {
      component.setName("name");

      assertEquals("name", component.getName());
      verify(bbjGroup, times(1)).setName(any());
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void whenControlThrowsBBjExceptionADwcjRuntimeExceptionIsThrown() throws BBjException {
      doThrow(BBjException.class).when(bbjGroup).setName(any());
      assertThrows(WebforjRuntimeException.class, () -> component.setName("name"));
    }
  }

  @Nested
  class ValueApi {

    @Test
    void shouldCheckByValue() {
      RadioButton[] buttons =
          {new RadioButton("Option 1", true), new RadioButton("Option 2", false)};
      component.add(buttons);

      component.setValue("Option 2");
      assertTrue(buttons[1].isChecked());
    }
  }

  @Nested
  class ValidationApi {
    RadioButton[] buttons = {mock(RadioButton.class), mock(RadioButton.class)};

    @BeforeEach
    void setup() throws IllegalAccessException {
      nullifyGroup();
      component.add(buttons);
    }

    @Test
    void shouldConfigureInvalid() {
      when(buttons[0].isInvalid()).thenReturn(true);

      component.setInvalid(true);
      verify(buttons[0], times(1)).setInvalid(true);
      verify(buttons[1], times(0)).setInvalid(true);
    }

    @Test
    void shouldConfigureInvalidWithMessage() {
      String message = "Invalid message";
      when(buttons[0].getInvalidMessage()).thenReturn(message);
      component.setInvalidMessage(message);

      verify(buttons[0], times(1)).setInvalidMessage(message);
      assertEquals(message, component.getInvalidMessage());
    }

    @Test
    void shouldConfigureValidationStyle() {
      List.of(buttons)
          .forEach(button -> when(button.getValidationStyle()).thenReturn(ValidationStyle.INLINE));

      component.setValidationStyle(ValidationStyle.INLINE);

      verify(buttons[0], times(1)).setValidationStyle(ValidationStyle.INLINE);
      verify(buttons[1], times(1)).setValidationStyle(ValidationStyle.INLINE);

      assertEquals(ValidationStyle.INLINE, component.getValidationStyle());
    }
  }
}
