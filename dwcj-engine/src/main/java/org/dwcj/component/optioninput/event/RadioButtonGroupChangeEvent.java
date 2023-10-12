package org.dwcj.component.optioninput.event;

import java.util.Map;
import org.dwcj.component.event.ComponentEvent;
import org.dwcj.component.optioninput.RadioButton;
import org.dwcj.component.optioninput.RadioButtonGroup;

/**
 * An event that is fired when the radio button group changes selection.
 *
 * @see RadioButtonGroup
 *
 * @author Hyyan Abo Fakher
 * @since 23.01
 */
public class RadioButtonGroupChangeEvent extends ComponentEvent<RadioButtonGroup> {

  /**
   * Creates a new event.
   *
   * @param component the component
   * @param payload the event map
   */
  public RadioButtonGroupChangeEvent(RadioButtonGroup component, Map<String, Object> payload) {
    super(component, payload);
  }

  /**
   * Returns the radio button that was checked.
   *
   * @return the radio button that was checked or null if none was checked
   */
  public RadioButton getChecked() {
    return (RadioButton) this.getEventMap().get("checked");
  }

  /**
   * Returns the radio button that was unchecked.
   *
   * @return the radio button that was unchecked or null if none was unchecked
   */
  public RadioButton getUnchecked() {
    return (RadioButton) this.getEventMap().get("unchecked");
  }
}

