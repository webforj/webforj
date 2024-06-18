package com.webforj.component.field;

import java.util.List;

/**
 * Represents a dwc field with a list spinner.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
interface HasOptions<T> {

  /**
   * Sets the list of options to be used by the spinner.
   *
   * @param list The list of options.
   * @return This component itself.
   */
  public T setOptions(List<String> list);

  /**
   * Gets the list of options used by the spinner.
   *
   * @return The list of options.
   */
  public List<String> getOptions();


  /**
   * Sets the index of the selected item in the spinner.
   *
   * @param index The index of the selected item.
   * @return This component itself.
   */
  public T setOptionIndex(int index);

  /**
   * Gets the index of the selected item in the spinner.
   *
   * @return The index of the selected item.
   */
  public int getOptionIndex();
}
