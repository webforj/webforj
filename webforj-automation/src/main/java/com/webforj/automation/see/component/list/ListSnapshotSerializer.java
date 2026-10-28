package com.webforj.automation.see.component.list;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.list.DwcList;
import com.webforj.component.list.ListItem;
import com.webforj.component.list.MultipleSelectableList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Serializes a list with its items and their keys, and the selected items when the snapshot
 * includes the browser state.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class ListSnapshotSerializer implements SnapshotSerializer<DwcList<?, ?>> {

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings("unchecked")
  public Class<DwcList<?, ?>> getContract() {
    return (Class<DwcList<?, ?>>) (Class<?>) DwcList.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void describe(DwcList<?, ?> list, SnapshotNode node) {
    List<ListItem> selected = node.isBrowserState() ? getSelectedItems(list) : List.of();
    if (list instanceof MultipleSelectableList<?> multiple
        && multiple.getSelectionMode() == MultipleSelectableList.SelectionMode.MULTIPLE) {
      node.addFlag("multiple");
    }

    String selectedTexts =
        selected.stream().map(ListItem::getText).collect(Collectors.joining(","));
    if (selectedTexts.equals(node.getText())) {
      node.setText(null);
    }

    List<ListItem> items = list.getItems();
    node.setItemCount(items.size());
    for (ListItem item : items.subList(0, Math.min(items.size(), node.getMaxWidth() + 1))) {
      SnapshotNode line =
          node.addItem("ListItem").setText(item.getText()).setAttribute("key", item.getKey());
      if (selected.contains(item)) {
        line.addFlag("selected");
        if (String.valueOf(item.getKey()).equals(node.getAttribute("value"))) {
          node.removeAttribute("value");
        }
      }
    }
  }

  private static List<ListItem> getSelectedItems(DwcList<?, ?> list) {
    if (list instanceof MultipleSelectableList<?> multiple) {
      return multiple.getSelectedItems();
    }

    ListItem selected = list.getSelected();

    return selected == null ? List.of() : List.of(selected);
  }
}
