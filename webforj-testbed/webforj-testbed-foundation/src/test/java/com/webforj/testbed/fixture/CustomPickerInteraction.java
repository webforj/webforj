package com.webforj.testbed.fixture;

import com.webforj.automation.act.CloseInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import java.util.function.Consumer;

/** What a user does with the custom picker, written the way an application writes it. */
public class CustomPickerInteraction implements CloseInteraction<CustomPicker> {

  /**
   * Returns the action that picks the given item and rejects an item the picker does not offer.
   *
   * @param item the item to pick
   *
   * @return the action to perform on the picker
   */
  public static Consumer<CustomPicker> pick(String item) {
    return picker -> {
      picker.select(item);
      if (picker.getSelected() == null) {
        throw new InteractionRejectedException(picker, "does not offer " + item);
      }
    };
  }

  @Override
  public Class<CustomPicker> getContract() {
    return CustomPicker.class;
  }

  @Override
  public void close(CustomPicker picker) {
    picker.deselect();
  }
}
