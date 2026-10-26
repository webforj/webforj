package com.webforj.automation.act.component.list;

import com.basis.startup.type.BBjVector;
import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.CloseInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.automation.act.DeselectInteraction;
import com.webforj.automation.act.ItemClickInteraction;
import com.webforj.automation.act.MouseOptions;
import com.webforj.automation.act.OpenInteraction;
import com.webforj.automation.act.SelectIndexInteraction;
import com.webforj.automation.act.SelectInteraction;
import com.webforj.automation.act.SelectKeyInteraction;
import com.webforj.component.list.DwcList;
import com.webforj.component.list.DwcSelectDropdown;
import com.webforj.component.list.ListItem;
import com.webforj.component.list.MultipleSelectableList;
import com.webforj.component.list.event.ListClickEvent;
import com.webforj.component.list.event.ListCloseEvent;
import com.webforj.component.list.event.ListOpenEvent;
import com.webforj.component.list.event.ListSelectEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Selects the items of lists, and opens, closes and clicks the items of the lists with a dropdown.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class ListInteraction extends AbstractComponentInteraction<DwcList<?, ?>>
    implements SelectInteraction<DwcList<?, ?>, Object>, SelectIndexInteraction<DwcList<?, ?>>,
    SelectKeyInteraction<DwcList<?, ?>>, DeselectInteraction<DwcList<?, ?>, Object>,
    OpenInteraction<DwcList<?, ?>>, CloseInteraction<DwcList<?, ?>>,
    ItemClickInteraction<DwcList<?, ?>, ListItem> {

  /**
   * Creates the interaction.
   */
  public ListInteraction() {
    super(DwcList.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void select(DwcList<?, ?> list, Object item) {
    try {
      list.select((ListItem) item);
    } catch (ClassCastException e) {
      throw reject(list, "does not select items of type " + item.getClass().getSimpleName());
    }

    if (!getSelectedItems(list).contains(item)) {
      throw reject(list, "does not contain " + item);
    }

    fireSelection(list);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void selectIndex(DwcList<?, ?> list, int index) {
    list.selectIndex(index);

    boolean selected = list instanceof MultipleSelectableList<?> multiple
        ? multiple.getSelectedIndices().contains(index)
        : list.getSelectedIndex() == index;
    if (!selected) {
      throw reject(list, "has no item at index " + index);
    }

    fireSelection(list);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void selectKey(DwcList<?, ?> list, Object key) {
    list.selectKey(key);

    boolean selected = list instanceof MultipleSelectableList<?> multiple
        ? multiple.getSelectedKeys().contains(key)
        : key.equals(list.getSelectedKey());
    if (!selected) {
      throw reject(list, "has no item with the key " + key);
    }

    fireSelection(list);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void deselect(DwcList<?, ?> list, Object item) {
    if (!(list instanceof MultipleSelectableList<?> multiple)) {
      throw reject(list, "does not deselect items");
    }

    if (!getSelectedItems(list).contains(item)) {
      return;
    }

    multiple.deselect((ListItem) item);
    fireSelection(list);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void open(DwcList<?, ?> list) {
    DwcSelectDropdown<?> dropdown = toDropdown(list, "has no dropdown to open");
    if (dropdown.isOpened()) {
      return;
    }

    dropdown.open();
    fire(new ListOpenEvent(dropdown, createCurrentSelection(dropdown)));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void close(DwcList<?, ?> list) {
    DwcSelectDropdown<?> dropdown = toDropdown(list, "has no dropdown to close");
    if (!dropdown.isOpened()) {
      return;
    }

    dropdown.close();
    fire(new ListCloseEvent(dropdown, createCurrentSelection(dropdown)));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void click(DwcList<?, ?> list, ListItem item, MouseOptions options) {
    DwcSelectDropdown<?> dropdown = toDropdown(list, "has no dropdown items to click");
    int index = dropdown.indexOf(item);
    if (index == -1) {
      throw reject(list, "does not contain the item '" + item.getText() + "'");
    }

    if (!dropdown.isOpened()) {
      throw reject(list, "is not open and a user cannot click its items");
    }

    dropdown.select(item);
    dropdown.close();
    fire(new ListClickEvent(dropdown, createSelection(List.of(index))));
    fire(new ListCloseEvent(dropdown, createCurrentSelection(dropdown)));
    fireSelection(dropdown);
  }

  private static DwcSelectDropdown<?> toDropdown(DwcList<?, ?> list, String reason) {
    if (!(list instanceof DwcSelectDropdown<?> dropdown)) {
      throw reject(list, reason);
    }

    return dropdown;
  }

  private static List<ListItem> getSelectedItems(DwcList<?, ?> list) {
    if (list instanceof MultipleSelectableList<?> multiple) {
      return List.copyOf(multiple.getSelectedItems());
    }

    ListItem selected = list.getSelected();

    return selected == null ? List.of() : List.of(selected);
  }

  private static void fireSelection(DwcList<?, ?> list) {
    fire(new ListSelectEvent<>(list, createCurrentSelection(list)));
  }

  private static Map<String, Object> createCurrentSelection(DwcList<?, ?> list) {
    List<Integer> indices =
        list instanceof MultipleSelectableList<?> multiple ? multiple.getSelectedIndices()
            : List.of(list.getSelectedIndex());
    List<Integer> selected = new ArrayList<>();
    for (int index : indices) {
      if (index >= 0) {
        selected.add(index);
      }
    }

    return createSelection(selected);
  }

  private static Map<String, Object> createSelection(List<Integer> indices) {
    Map<String, Object> data = new HashMap<>();
    data.put("index", indices.isEmpty() ? -1 : indices.get(0));
    data.put("indices", new BBjVector(new ArrayList<>(indices)));

    return data;
  }
}
