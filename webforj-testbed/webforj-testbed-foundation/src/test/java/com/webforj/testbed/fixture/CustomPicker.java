package com.webforj.testbed.fixture;

import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.data.selection.SingleSelectable;
import java.util.List;

/** Provides a custom selection component for interaction tests. */
@NodeName("x-picker")
public class CustomPicker extends ElementComposite
    implements SingleSelectable<CustomPicker, String> {
  private final List<String> items;
  private int selectedIndex = -1;

  public CustomPicker(String... items) {
    this.items = List.of(items);
  }

  @Override
  public CustomPicker selectIndex(int index) {
    selectedIndex = index >= 0 && index < items.size() ? index : -1;

    return this;
  }

  @Override
  public CustomPicker selectKey(Object key) {
    return selectIndex(items.indexOf(key));
  }

  @Override
  public CustomPicker select(String item) {
    return selectKey(item);
  }

  @Override
  public CustomPicker deselect() {
    return selectIndex(-1);
  }

  @Override
  public int getSelectedIndex() {
    return selectedIndex;
  }

  @Override
  public Object getSelectedKey() {
    return getSelected();
  }

  @Override
  public String getSelected() {
    return selectedIndex == -1 ? null : items.get(selectedIndex);
  }
}
