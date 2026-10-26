package com.webforj.automation.act;

import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.data.selection.ItemMultiSelectable;
import java.util.ArrayList;
import java.util.List;

/**
 * Multiple selection composite used to test components that bring their own contract.
 */
@NodeName("x-basket")
public final class CustomBasket extends ElementComposite
    implements ItemMultiSelectable<CustomBasket, String> {
  private final List<String> items;
  private final List<String> selected = new ArrayList<>();

  /**
   * Creates a basket offering the given items.
   *
   * @param items the items on offer
   */
  public CustomBasket(String... items) {
    this.items = List.of(items);
  }

  @Override
  public CustomBasket select(String... picked) {
    for (String item : picked) {
      if (items.contains(item) && !selected.contains(item)) {
        selected.add(item);
      }
    }

    return this;
  }

  @Override
  public CustomBasket deselect(String... picked) {
    selected.removeAll(List.of(picked));
    return this;
  }

  @Override
  public List<String> getSelectedItems() {
    return List.copyOf(selected);
  }
}
