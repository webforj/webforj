package com.webforj.testbed.fixture;

import com.webforj.testbed.assertion.AbstractComponentAssert;

/** Assertions on the custom picker, written the way an application writes them. */
public final class CustomPickerAssert
    extends AbstractComponentAssert<CustomPickerAssert, CustomPicker> {

  private CustomPickerAssert(CustomPicker actual) {
    super(actual, CustomPickerAssert.class);
  }

  public static CustomPickerAssert verifyThat(CustomPicker actual) {
    return new CustomPickerAssert(actual);
  }

  /**
   * Verifies that the picker holds the given item as its selection.
   *
   * @param item the expected selected item
   *
   * @return this assertion
   */
  public CustomPickerAssert hasPicked(String item) {
    isNotNull();
    if (!item.equals(actual.getSelected())) {
      failWithMessage("Expected %s to have picked %s but it picked %s.", componentName(), item,
          actual.getSelected());
    }

    return myself;
  }
}
