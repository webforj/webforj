package org.dwcj.component.list;

import com.basis.bbj.proxies.sysgui.BBjListBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.event.ComponentEventListener;
import org.dwcj.component.list.event.ListSelectEvent;
import org.dwcj.component.window.Window;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.BBjFunctionalityHelper;

/**
 * Represents a component that displays a list of objects and allows users to select single or
 * multiple items from the list.
 *
 * <p>
 * The ListBox class is a UI component that presents a scrollable list of items from which users can
 * make multiple selections. It provides methods to set and retrieve the selection mode (single or
 * multiple), select and deselect items in the list, and get the currently selected items.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public final class ListBox extends DwcList<ListBox> implements MultipleSelectableList<ListBox> {

  private SelectionMode selectionMode = SelectionMode.SINGLE;

  /**
   * Constructs a new ListBox.
   */
  public ListBox() {
    super();
  }

  /**
   * Constructs a new ListBox with the given label.
   *
   * @param label the label
   */
  public ListBox(String label) {
    super(label);
  }

  /**
   * Constructs a new ListBox with the given label and select listener.
   *
   * @param label the label of the component
   * @param selectListener the listener to be called when the user selects an item
   */
  public ListBox(String label, ComponentEventListener<ListSelectEvent> selectListener) {
    super(label, selectListener);
  }

  /**
   * {@inheritDoc}
   */
  public ListBox setSelectionMode(SelectionMode mode) {
    BBjListBox list = inferListBox();

    if (list != null) {
      try {
        list.setMultipleSelection(mode == SelectionMode.MULTIPLE);
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    this.selectionMode = mode;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  public SelectionMode getSelectionMode() {
    return this.selectionMode;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListBox deselect() {
    if (size() > 0) {
      int selectedIndex = getSelectedIndex();
      if (selectedIndex != -1) {
        ListItem item = getByIndex(selectedIndex);
        return doDeselect(item);
      }
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListBox deselect(ListItem item) {
    if (selectionMode == SelectionMode.SINGLE) {
      throw new IllegalStateException("Cannot deselect a given item in single selection mode");
    }

    return doDeselect(item);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListBox deselectKey(Object key) {
    ListItem item = getByKey(key);
    return deselect(item);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListBox deselectIndex(int index) {
    ListItem item = getByIndex(index);
    return deselect(item);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListBox deselectAll() {
    getInternalSelectedList().clear();

    BBjListBox list = inferListBox();

    if (list != null) {
      try {
        list.deselectAll();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListBox select(ListItem... items) {
    if (selectionMode == SelectionMode.SINGLE && size() > 1) {
      throw new IllegalStateException("Cannot select more than one item in single selection mode");
    }

    for (ListItem item : items) {
      verifyItemInList(item);
    }

    // add the items to the selected items map if they are not already there
    Arrays.stream(items).forEach(item -> {
      if (!getInternalSelectedList().contains(item)) {
        getInternalSelectedList().add(item);
      }
    });

    BBjListBox list = inferListBox();

    if (list != null) {
      try {
        Collection<Integer> indices =
            Arrays.stream(items).map(item -> indexOf(item.getKey())).toList();
        list.setSelectedIndices(new BBjVector(indices));
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListBox selectKeys(Object... keys) {
    for (Object key : keys) {
      verifyItemInList(key);
    }

    ListItem[] listItem = Arrays.stream(keys).map(key -> getByKey(key)).toArray(ListItem[]::new);
    return select(listItem);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListBox selectIndices(int... indices) {
    for (int index : indices) {
      verifyValidIndex(index, false);
    }

    ListItem[] listItem =
        Arrays.stream(indices).mapToObj(index -> getByIndex(index)).toArray(ListItem[]::new);
    return select(listItem);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<ListItem> getSelectedItems() {
    List<Integer> indices = getSelectedIndices();
    return indices.stream().map(i -> getByIndex(i.intValue())).filter(Objects::nonNull).toList();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<Object> getSelectedKeys() {
    List<ListItem> items = getSelectedItems();
    return items.stream().map(ListItem::getKey).toList();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<Integer> getSelectedIndices() {
    BBjListBox list = inferListBox();

    if (list != null) {
      try {
        return Arrays.stream(list.getSelectedIndices().toArray())
            .map(i -> Integer.valueOf(i.toString())).collect(Collectors.toList());
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    return getInternalSelectedList().stream().map(i -> indexOf(i)).toList();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> getRestrictedProperties() {
    List<String> properties = super.getRestrictedProperties();
    properties.addAll(Arrays.asList("allowDeselection", "autoValidate", "autoValidateOnLoad",
        "autoWasValidated", "disabled", "expanse", "hasFocus", "invalid", "invalidMessage", "items",
        "label", "multiSelection", "multiSelectionByClick", "readonly", "renderer", "selected",
        "tabTraversable", "typeToSelect", "typeToSelectCaseSensitive", "typeToSelectTimeout",
        "valid", "validationIcon", "validationPopoverDistance", "validationPopoverPlacement",
        "validationPopoverSkidding", "validationStyle", "validator"));

    return properties;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      setControl(w.addListBox("", flags));
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to create the BBjListBox Control", e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    super.onAttach();

    catchupItemsChanges();

    if (selectionMode != SelectionMode.SINGLE) {
      setSelectionMode(selectionMode);
    }

    if (!getInternalSelectedList().isEmpty()) {
      if (getSelectionMode() == SelectionMode.SINGLE) {
        catchupSingleSelection();
      } else {
        select(getInternalSelectedList().toArray(new ListItem[0]));
      }
    }
  }

  private BBjListBox inferListBox() {
    try {
      return (BBjListBox) ComponentAccessor.getDefault().getControl(this);
    } catch (IllegalAccessException e) {
      throw new DwcjRuntimeException(e);
    }
  }

  /**
   * Deselects the given item.
   *
   * @param item the item to deselect
   *
   * @return the component itself
   * @throws IllegalArgumentException if the given item is not in the list
   */
  private ListBox doDeselect(ListItem item) {
    verifyItemInList(item);

    BBjListBox list = inferListBox();

    if (list != null) {
      try {
        int index = indexOf(item.getKey());
        if (index != -1) {
          list.deselectIndex(index);
        }
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    getInternalSelectedList().remove(item);
    return this;
  }
}
