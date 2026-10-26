package com.webforj.testbed.fixture;

import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;

/** Serializes the custom picker for snapshot tests. */
public class CustomPickerSnapshotSerializer implements SnapshotSerializer<CustomPicker> {

  @Override
  public Class<CustomPicker> getContract() {
    return CustomPicker.class;
  }

  @Override
  public void describe(CustomPicker picker, SnapshotNode node) {
    node.setAttribute("selected", picker.getSelected());
  }
}
