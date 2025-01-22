package com.webforj.component.list;

import com.basis.bbj.proxies.sysgui.BBjListBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.list.event.ListSelectEvent;
import com.webforj.component.window.Window;
import com.webforj.data.binding.Binding;
import com.webforj.data.binding.BindingContext;
import com.webforj.data.binding.concern.BindAware;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
public final class ListBox extends DwcList<ListBox, List<Object>>
    implements MultipleSelectableList<ListBox>, BindAware {

  private SelectionMode selectionMode = SelectionMode.SINGLE;
  private boolean registeredSelectValueChangeListener = false;

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
  public ListBox(String label, EventListener<ListSelectEvent<List<Object>>> selectListener) {
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
        throw new WebforjRuntimeException(e);
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
  public ListBox deselect(ListItem... item) {
    for (ListItem i : item) {
      doDeselect(i);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListBox deselectKey(Object... key) {
    for (Object k : key) {
      ListItem item = getByKey(k);
      deselect(item);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListBox deselectIndex(int... index) {
    for (int i : index) {
      ListItem item = getByIndex(i);
      deselect(item);
    }

    return this;
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
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListBox select(ListItem... items) {
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
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListBox selectKey(Object... keys) {
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
  public ListBox selectIndex(int... indices) {
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
        throw new WebforjRuntimeException(e);
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
   * Alias for {@link #setItems(ListItem...)}.
   *
   * @param value the items to set
   * @return the component itself
   */
  @Override
  public ListBox setValue(List<Object> value) {
    try {
      return selectKey(value.toArray());
    } catch (IllegalArgumentException e) {
      // pass
    }

    return this;
  }

  /**
   * Alias for {@link #getSelectedItems()}.
   *
   * @return the selected items
   */
  @Override
  public List<Object> getValue() {
    return getSelectedKeys();
  }

  /**
   * Selects the items with the given text.
   *
   * <p>
   * Selects one or more items with the given text. For multiple selection mode, the text should be
   * separated by a comma. If the text is not found in the list, it will be ignored.
   * </p>
   *
   * @return the component itself
   */
  @Override
  public ListBox setText(String text) {
    Objects.requireNonNull(text, "The text cannot be null");

    List<String> items = Arrays.asList(text.trim().split(",\\s*"));

    // find all items with the given text
    Stream<ListItem> matched = getItems().stream().filter(i -> items.contains(i.getText()));
    SelectionMode mode = getSelectionMode();

    if (mode.equals(SelectionMode.SINGLE)) {
      matched.findFirst().ifPresent(this::select);
    } else if (mode.equals(SelectionMode.MULTIPLE)) {
      select(matched.toArray(ListItem[]::new));
    }

    return this;
  }

  /**
   * Returns the text of the selected items separated by a comma.
   *
   * <p>
   * If no item is selected, then null is returned.
   * </p>
   *
   * @return the text of the selected items
   */
  @Override
  public String getText() {
    List<ListItem> items = getSelectedItems();
    if (items.isEmpty()) {
      return null;
    }

    return items.stream().map(ListItem::getText).collect(Collectors.joining(","));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListenerRegistration<ValueChangeEvent<List<Object>>> addValueChangeListener(
      EventListener<ValueChangeEvent<List<Object>>> listener) {
    ListenerRegistration<ValueChangeEvent<List<Object>>> registration =
        getEventDispatcher().addListener(ValueChangeEvent.class, listener);

    if (!registeredSelectValueChangeListener) {
      addSelectListener(ev -> {
        List<Object> keys = ev.getSelectedItems().stream().map(ListItem::getKey).toList();
        ValueChangeEvent<List<Object>> valueChangeEvent = new ValueChangeEvent<>(this, keys);
        getEventDispatcher().dispatchEvent(valueChangeEvent);
      });

      registeredSelectValueChangeListener = true;
    }

    return registration;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public <B> void onBind(BindingContext<B> context, Class<B> beanClass, String propertyName) {
    if (!context.getBinding(propertyName).getTransformer().isPresent()) {
      Binding<ListBox, List<Object>, B, List<String>> binding =
          (Binding<ListBox, List<Object>, B, List<String>>) context.getBinding(this);
      binding.setTransformer(new TypeTransformer<>());
    }
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
      throw new WebforjRuntimeException("Failed to create the BBjListBox Control", e);
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
      select(getInternalSelectedList().toArray(new ListItem[0]));
    }
  }

  private BBjListBox inferListBox() {
    try {
      return (BBjListBox) ComponentAccessor.getDefault().getControl(this);
    } catch (IllegalAccessException e) {
      throw new WebforjRuntimeException(e);
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
        throw new WebforjRuntimeException(e);
      }
    }

    getInternalSelectedList().remove(item);
    return this;
  }
}
