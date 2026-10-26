package com.webforj.automation.act;

import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.data.selection.SingleSelectable;
import java.util.List;

/**
 * Single selection composite used to test components that bring their own contract.
 */
@NodeName("x-picker")
public final class CustomPicker extends ElementComposite
    implements SingleSelectable<CustomPicker, String> {
  private final List<String> items;
  private int selectedIndex = -1;

  /**
   * Creates a picker offering the given items.
   *
   * @param items the items on offer
   */
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
