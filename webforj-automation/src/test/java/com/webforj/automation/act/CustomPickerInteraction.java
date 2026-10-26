package com.webforj.automation.act;

import com.webforj.automation.act.exception.InteractionRejectedException;
import java.util.function.Consumer;

/**
 * Closes a {@link CustomPicker} as an application interaction.
 */
public final class CustomPickerInteraction implements CloseInteraction<CustomPicker> {

  @Override
  public Class<CustomPicker> getContract() {
    return CustomPicker.class;
  }

  @Override
  public void close(CustomPicker picker) {
    picker.deselect();
  }

  /**
   * Selects the given item and rejects the interaction when the picker does not offer it.
   *
   * @param item the item to pick
   * @return the interaction
   */
  public static Consumer<CustomPicker> pick(String item) {
    return picker -> {
      picker.select(item);
      if (picker.getSelected() == null) {
        throw new InteractionRejectedException(picker, "does not offer " + item);
      }
    };
  }
}
