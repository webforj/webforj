package com.webforj.component.field;

import com.basis.bbj.proxies.sysgui.BBjListSpinner;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.ArrayList;
import java.util.List;

/**
 * Represents a mixin for a dwc field with a list spinner.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
class HasOptionsMixin implements HasOptions<HasOptionsMixin> {
  private final DwcField<?, ?> field;
  private List<String> list = new ArrayList<>();
  private int optionIndex = 0;

  /**
   * Creates a new instance of the mixin.
   *
   * @param field The field to which the mixin is attached.
   */
  HasOptionsMixin(DwcField<?, ?> field) {
    this.field = field;
  }

  /**
   * Sets the list of options to be used by the spinner.
   *
   * @param list The list of options.
   */
  public HasOptionsMixin setOptions(List<String> list) {
    this.list = list;
    BBjListSpinner spinner = inferListSpinner();

    if (spinner != null) {
      try {
        BBjVector listVector = new BBjVector();
        for (String item : list) {
          listVector.add(item);
        }

        spinner.setSpinList(listVector);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Gets the list of options used by the spinner.
   *
   * @return The list of options.
   */
  public List<String> getOptions() {
    return list;
  }

  /**
   * Sets the index of the selected item in the spinner.
   *
   * @param index The index of the selected item.
   * @return This component itself.
   */
  public HasOptionsMixin setOptionIndex(int index) {
    int size = list.size();
    this.optionIndex = size > 0 ? index % size : 0;
    BBjListSpinner spinner = inferListSpinner();

    if (spinner != null) {
      try {
        spinner.setListIndex(index);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Gets the index of the selected item in the spinner.
   *
   * @return The index of the selected item.
   */
  public int getOptionIndex() {
    BBjListSpinner spinner = inferListSpinner();

    if (spinner != null) {
      try {
        return spinner.getListIndex();
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this.optionIndex;
  }

  void onAttach() {
    if (list != null && !list.isEmpty()) {
      setOptions(list);
    }

    if (optionIndex != 0) {
      setOptionIndex(optionIndex);
    }
  }

  BBjListSpinner inferListSpinner() {
    try {
      return (BBjListSpinner) ComponentAccessor.getDefault().getControl(field);
    } catch (IllegalAccessException e) {
      throw new WebforjRuntimeException(e);
    }
  }
}
