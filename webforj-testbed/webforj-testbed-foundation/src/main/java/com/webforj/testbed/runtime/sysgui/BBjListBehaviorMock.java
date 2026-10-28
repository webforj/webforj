package com.webforj.testbed.runtime.sysgui;

import com.basis.startup.type.BBjException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * The mock backing the shared list control surface.
 *
 * <p>
 * Driven by {@code com.webforj.component.list.DwcList}, which pushes items as display texts,
 * selects one index and reads the selected index back, expecting minus one when nothing is
 * selected. Every list control mock extends this class, matching how the list control interfaces
 * extend the shared list interface.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjListBehaviorMock extends BBjControlMock {
  private final List<String> items = new ArrayList<>();
  private final Set<Integer> selectedIndices = new LinkedHashSet<>();
  private boolean multipleSelection = false;

  /**
   * Creates a new list control mock.
   *
   * @param id the control identifier
   */
  BBjListBehaviorMock(int id) {
    super(id);
  }

  /**
   * Returns the items pushed through the control, in order.
   *
   * @return the item texts
   */
  public List<String> getItems() {
    return List.copyOf(items);
  }

  /**
   * Selects the item at the given index, replacing the current selection.
   *
   * @param index the index to select
   */
  public void selectIndex(int index) {
    // The runtime drops the selection for a negative index and ignores one past the end.
    if (index < 0) {
      selectedIndices.clear();
      return;
    }

    if (index >= items.size()) {
      return;
    }

    if (!multipleSelection) {
      selectedIndices.clear();
    }
    selectedIndices.add(index);
  }

  void setMultipleSelection(boolean multipleSelection) {
    this.multipleSelection = multipleSelection;
  }

  /**
   * Selects exactly the given indices, replacing the current selection.
   *
   * @param indices the indices to select
   */
  public void selectIndices(Collection<Integer> indices) {
    selectedIndices.clear();
    selectedIndices.addAll(indices);
  }

  /**
   * Returns the selected indices, in selection order.
   *
   * @return the selected indices
   */
  protected Set<Integer> getSelectedIndicesState() {
    return selectedIndices;
  }

  /**
   * Replaces the text of the item at the given index.
   *
   * @param index the item index
   * @param text the new item text
   */
  protected void setItemText(int index, String text) throws BBjException {
    items.set(checkIndex(index), text);
  }

  private int checkIndex(int index) throws BBjException {
    if (index < 0 || index >= items.size()) {
      throw new BBjException("Invalid index " + index, BBjException.ARG_ERR);
    }

    return index;
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "addItem" -> {
        items.add((String) args[0]);
        yield null;
      }
      case "insertItemAt" -> {
        // The runtime rejects a negative index and appends for one past the end.
        int index = (Integer) args[0];
        if (index < 0) {
          throw new BBjException("Invalid index " + index, BBjException.ARG_ERR);
        }
        items.add(Math.min(index, items.size()), (String) args[1]);
        yield null;
      }
      case "insertItems" -> {
        List<?> inserted = args[1] instanceof List<?> list ? list : List.of((String) args[1]);
        int index = (Integer) args[0];
        index = index < 0 || index > items.size() ? items.size() : index;
        for (Object item : inserted) {
          items.add(index++, String.valueOf(item));
        }
        yield null;
      }
      case "removeItemAt" -> {
        int removed = checkIndex((Integer) args[0]);
        items.remove(removed);
        Set<Integer> shifted = new LinkedHashSet<>();
        for (Integer selected : selectedIndices) {
          if (selected != removed) {
            shifted.add(selected > removed ? selected - 1 : selected);
          }
        }
        selectedIndices.clear();
        selectedIndices.addAll(shifted);
        yield null;
      }
      case "removeAllItems" -> {
        items.clear();
        selectedIndices.clear();
        yield null;
      }
      case "getItemAt" -> items.get(checkIndex((Integer) args[0]));
      case "getItemCount" -> items.size();
      case "indexOf" -> items.indexOf(args[0]);
      case "selectIndex" -> {
        selectIndex((Integer) args[0]);
        yield null;
      }
      case "getSelectedIndex" -> selectedIndices.isEmpty() ? -1
          : selectedIndices.iterator().next();
      case "getSelectedItem" -> {
        int selected = selectedIndices.isEmpty() ? -1 : selectedIndices.iterator().next();
        yield selected >= 0 && selected < items.size() ? items.get(selected) : "";
      }
      default -> super.handle(proxy, method, args);
    };
  }
}
