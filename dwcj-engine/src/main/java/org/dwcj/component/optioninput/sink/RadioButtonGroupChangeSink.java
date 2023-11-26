package org.dwcj.component.optioninput.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjSelectionChangeEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjRadioButton;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.basis.startup.type.BBjException;
import java.util.HashMap;
import java.util.List;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.optioninput.RadioButton;
import org.dwcj.component.optioninput.RadioButtonGroup;
import org.dwcj.component.optioninput.event.RadioButtonGroupChangeEvent;
import org.dwcj.dispatcher.EventDispatcher;

/**
 * RadioButtonGroupChangeSink maps the BBjSelectionChangeEvent event to the Java
 * {@link RadioButtonGroupChangeEvent} event.
 *
 * @see RadioButtonGroup
 * @see RadioButtonGroupChangeEvent
 *
 * @author Hyyan Abo Fakher
 * @since 23.01
 */
public class RadioButtonGroupChangeSink extends AbstractRadioButtonEventSink {

  public RadioButtonGroupChangeSink(RadioButtonGroup component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_SELECTION_CHANGE);
  }

  /**
   * Handles the BBj event and dispatches a new {@link RadioButtonGroupChangeEvent}.
   *
   * @param ev A BBj BBjSelectionChangeEvent event
   */
  public void handleEvent(BBjEvent ev) {
    BBjSelectionChangeEvent event = (BBjSelectionChangeEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("checked", getComponentFromControl(event.getSelected()));
    map.put("unchecked", getComponentFromControl(event.getDeselected()));

    RadioButtonGroupChangeEvent dwcEv = new RadioButtonGroupChangeEvent(getComponent(), map);
    getEventDispatcher().dispatchEvent(dwcEv);
  }

  BBjControl getControl(RadioButton button) throws IllegalAccessException {
    return ComponentAccessor.getDefault().getControl(button);
  }

  private RadioButton getComponentFromControl(BBjRadioButton control) {
    if (control != null) {
      List<RadioButton> buttons = getComponent().getRadioButtons();

      for (RadioButton button : buttons) {
        try {
          BBjControl buttonControl = getControl(button);
          if (buttonControl != null && buttonControl.getID() == control.getID()) {
            return button;
          }
        } catch (BBjException | IllegalAccessException e) {
          // pass
        }
      }
    }

    return null;
  }
}
