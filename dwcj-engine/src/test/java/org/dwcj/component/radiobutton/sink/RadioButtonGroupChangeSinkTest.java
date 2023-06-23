package org.dwcj.component.radiobutton.sink;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjSelectionChangeEvent;
import com.basis.bbj.proxies.sysgui.BBjRadioButton;
import com.basis.startup.type.BBjException;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.radiobutton.RadioButton;
import org.dwcj.component.radiobutton.RadioButtonGroup;
import org.dwcj.component.radiobutton.event.RadioButtonGroupChangeEvent;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class RadioButtonGroupChangeSinkTest {

  RadioButtonGroupChangeEvent dispatchedEvent;

  @Test
  void dispatchRadioButtonGroupChangeEvent() throws BBjException, IllegalAccessException {
    BBjRadioButton selectedControl = mock(BBjRadioButton.class);
    when(selectedControl.getID()).thenReturn(1);

    BBjRadioButton deselectedControl = mock(BBjRadioButton.class);
    when(deselectedControl.getID()).thenReturn(2);

    BBjSelectionChangeEvent event = mock(BBjSelectionChangeEvent.class);
    when(event.getSelected()).thenReturn(selectedControl);
    when(event.getDeselected()).thenReturn(deselectedControl);

    RadioButton selectedComponent = mock(RadioButton.class);
    RadioButton deselectedComponent = mock(RadioButton.class);
    RadioButtonGroup component = new RadioButtonGroup(selectedComponent, deselectedComponent);

    EventDispatcher dispatcher = new EventDispatcher();
    dispatcher.addEventListener(RadioButtonGroupChangeEvent.class, e -> dispatchedEvent = e);

    RadioButtonGroupChangeSink sink = spy(new RadioButtonGroupChangeSink(component, dispatcher));
    when(sink.getControl(selectedComponent)).thenReturn(selectedControl);
    when(sink.getControl(deselectedComponent)).thenReturn(deselectedControl);

    sink.handleEvent(event);

    assertEquals(selectedComponent, dispatchedEvent.getChecked());
    assertEquals(deselectedComponent, dispatchedEvent.getUnchecked());
  }
}
