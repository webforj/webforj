package org.dwcj.component.list;

import com.basis.bbj.proxies.sysgui.BBjComboBox;
import com.basis.bbj.proxies.sysgui.ListBehavior;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.DwcFocusableComponent;
import org.dwcj.component.Expanse;
import org.dwcj.component.event.EventSinkListenerRegistry;
import org.dwcj.component.list.event.ListSelectEvent;
import org.dwcj.component.list.sink.ListSelectEventSink;
import org.dwcj.concern.HasExpanse;
import org.dwcj.concern.HasHorizontalAlignment;
import org.dwcj.concern.HasLabel;
import org.dwcj.dispatcher.EventListener;
import org.dwcj.dispatcher.ListenerRegistration;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * An abstract class representing a list of items, where each item is associated with a key and a
 * value. It provides methods to add, insert, remove, select, and manage the list's items. This
 * class is used as a foundation for creating various list components.
 *
 * @param <T> The type of the implementing component.
 *
 * @see ListItem
 * @see ListBox
 * @see ComboBox
 * @see ChoiceBox
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public abstract class DwcList<T extends DwcFocusableComponent<T>> extends DwcFocusableComponent<T>
    implements Iterable<ListItem>, HasLabel<T>, HasExpanse<T, Expanse>, HasHorizontalAlignment<T>,
    SelectableList<T> {
  private static final String DUPLICATION_ITEM_MESSAGE = "Item is already in the list";
  private static final String ITEM_MISSING_MESSAGE = "Item is not in the list";
  private static final String INVALID_INDEX_MESSAGE = "Invalid item index '%d'";
  private static final String NULL_ITEM_MESSAGE = "Item cannot be null";

  private final LinkedHashMap<Object, ListItem> items = new LinkedHashMap<>();
  private final List<ListItem> selectedItems = new ArrayList<>();

  private final EventSinkListenerRegistry<ListSelectEvent> selectEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new ListSelectEventSink(this, getEventDispatcher()),
          ListSelectEvent.class);

  private String label = "";

  protected DwcList() {
    super();
    setExpanse(Expanse.MEDIUM);
  }

  /**
   * Constructs a new DwcList with a given label.
   *
   * @param label the label of the component
   */
  protected DwcList(String label) {
    this();
    setLabel(label);
  }

  /**
   * Constructs a new DwcList with a given label and selection event listener.
   *
   * @param label the label of the component
   * @param selectListener the listener to be called when the user selects an item
   */
  protected DwcList(String label, EventListener<ListSelectEvent> selectListener) {
    this(label);
    addSelectListener(selectListener);
  }

  /**
   * Adds an item to the list.
   *
   * @param item The item to be added.
   * @return The added item instance.
   *
   * @throws NullPointerException If the item is null.
   * @throws IllegalArgumentException If the item is already in the list.
   */
  public ListItem add(ListItem item) {
    verifyItemNotInList(item);

    items.put(item.getKey(), item);
    item.addPropertyChangeListener(new ListItemChangeListener());

    ListBehavior list = inferList();

    if (list != null) {
      try {
        list.addItem(item.getText());
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    return item;
  }

  /**
   * Adds an item to the list with the specified key and value.
   *
   * @param key The key of the item.
   * @param text The item to be added.
   * @return The added item instance.
   * @throws NullPointerException If the key is null.
   * @throws IllegalArgumentException If the key is already in the list.
   */
  public ListItem add(Object key, String text) {
    return add(new ListItem(key, text));
  }

  /**
   * Adds an item to the list with the specified value.
   *
   * @param item The item to be added.
   * @return The added item instance.
   */
  public ListItem add(String item) {
    return add(new ListItem(item));
  }

  /**
   * Inserts an item at the specified index with the given key and value.
   *
   * @param index The index at which the item should be inserted.
   * @param item The item to be inserted.
   * @return The inserted item instance.
   * @throws IndexOutOfBoundsException If the index is out of range.
   * @throws NullPointerException If the item is null.
   * @throws IllegalArgumentException If the item is already in the list.
   */
  public ListItem insert(int index, ListItem item) {
    verifyValidIndex(index, true);
    verifyItemNotInList(item);

    if (index == items.size()) {
      items.put(item.getKey(), item);
    } else {
      List<Map.Entry<Object, ListItem>> entryList = new ArrayList<>(items.entrySet());
      entryList.add(index, new AbstractMap.SimpleEntry<>(item.getKey(), item));

      items.clear();
      for (Map.Entry<Object, ListItem> entry : entryList) {
        items.put(entry.getKey(), entry.getValue());
      }
    }

    item.addPropertyChangeListener(new ListItemChangeListener());

    ListBehavior list = inferList();

    if (list != null) {
      try {
        list.insertItemAt(index, item.getText());
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }
    return item;
  }

  /**
   * Inserts an item at the specified index with the given key and value.
   *
   * @param index The index at which the item should be inserted.
   * @param key The key of the item to be inserted.
   * @param text The text of the item to be inserted.
   * @return The inserted item instance.
   * @throws IndexOutOfBoundsException If the index is out of range.
   * @throws NullPointerException If the key is null.
   * @throws IllegalArgumentException If the key is already in the list.
   */
  public ListItem insert(int index, Object key, String text) {
    return insert(index, new ListItem(key, text));
  }

  /**
   * Inserts an item at the specified index with the given value.
   *
   * @param index The index at which the item should be inserted.
   * @param text The text of the item to be inserted.
   * @return The inserted item instance.
   * @throws IndexOutOfBoundsException If the index is out of range.
   */
  public ListItem insert(int index, String text) {
    return insert(index, new ListItem(text));
  }

  /**
   * Inserts multiple items at the specified index.
   *
   * @param index The index at which the items should be inserted.
   * @param items The list containing the items to be inserted.
   * @return The component itself.
   * @throws IndexOutOfBoundsException If the index is out of range.
   * @throws NullPointerException If the items list is null or contains a null item.
   * @throws IllegalArgumentException If the items list contains an item with a key that is already
   *         in the list.
   */
  public T insert(int index, List<ListItem> items) {
    verifyValidIndex(index, true);

    if (items == null) {
      throw new NullPointerException("Items list cannot be null");
    }

    for (ListItem item : items) {
      verifyItemNotInList(item);
    }

    List<Map.Entry<Object, ListItem>> entryList = new ArrayList<>(this.items.entrySet());
    int i = index;
    for (ListItem item : items) {
      entryList.add(i, new AbstractMap.SimpleEntry<>(item.getKey(), item));
      i++;
    }

    this.items.clear();
    for (Map.Entry<Object, ListItem> entry : entryList) {
      ListItem item = entry.getValue();
      this.items.put(entry.getKey(), item);
      item.addPropertyChangeListener(new ListItemChangeListener());
    }

    doBulkInsert(index, items);

    return getSelf();
  }

  /**
   * Inserts multiple items at the end of the list.
   *
   * @param items The list containing the items to be inserted.
   * @return The component itself.
   * @throws NullPointerException If the items list is null or contains a null item.
   * @throws IllegalArgumentException If the items list contains an item with a key that is already
   *         in the list.
   */
  public T insert(List<ListItem> items) {
    return insert(size(), items);
  }

  /**
   * Inserts multiple items at the specified index.
   *
   * @param index The index at which the items should be inserted.
   * @param items The list containing the items to be inserted.
   * @return The component itself.
   * @throws IndexOutOfBoundsException If the index is out of range.
   * @throws NullPointerException If the items list is null or contains a null item.
   * @throws IllegalArgumentException If the items list contains an item with a key that is already
   *         in the list.
   */
  public T insert(int index, String... items) {
    return insert(index,
        Arrays.stream(items).map(item -> new ListItem(UUID.randomUUID(), item)).toList());
  }

  /**
   * Inserts multiple items at the end of the list.
   *
   * @param items The list containing the items to be inserted.
   * @return The component itself.
   * @throws NullPointerException If the items list is null or contains a null item.
   * @throws IllegalArgumentException If the items list contains an item with a key that is already
   *         in the list.
   */
  public T insert(String... items) {
    return insert(size(), items);
  }

  /**
   * Returns the index of the item with the specified key.
   *
   * @param key The key of the item to search for.
   * @return The index of the item, or -1 if the key is not found.
   */
  public int indexOf(Object key) {
    int index = 0;
    for (Map.Entry<Object, ListItem> entry : items.entrySet()) {
      if (entry.getKey().equals(key)) {
        return index;
      }
      index++;
    }

    return -1;
  }

  /**
   * Returns the index of the specified item.
   *
   * @param item The item to search for.
   * @return The index of the item, or -1 if the item is not found.
   */
  public int indexOf(ListItem item) {
    return indexOf(item.getKey());
  }

  /**
   * Removes the item at the specified index from the list.
   *
   * @param index The index of the item to remove.
   * @return The component itself.
   * @throws IndexOutOfBoundsException If the index is out of range.
   */
  public T remove(int index) {
    verifyValidIndex(index, false);

    List<Map.Entry<Object, ListItem>> entryList = new ArrayList<>(items.entrySet());
    entryList.remove(index);

    items.clear();
    for (Map.Entry<Object, ListItem> entry : entryList) {
      items.put(entry.getKey(), entry.getValue());
    }

    ListBehavior list = inferList();
    if (list != null) {
      try {
        list.removeItemAt(index);
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    return getSelf();
  }

  /**
   * Removes the item with the specified key from the list.
   *
   * @param key The key of the item to remove.
   * @return The component itself.
   */
  public T remove(Object key) {
    return remove(indexOf(key));
  }

  /**
   * Removes all items from the list.
   *
   * @return The component itself.
   */
  public T removeAll() {
    items.clear();

    ListBehavior list = inferList();
    if (list != null) {
      try {
        list.removeAllItems();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    return getSelf();
  }

  /**
   * Returns the item associated with the specified key.
   *
   * @param key The key of the item to retrieve.
   * @return The item, or null if the key is not found.
   */
  public ListItem getByKey(Object key) {
    return items.get(key);
  }

  /**
   * Alias for {@link #getByKey(Object)}.
   *
   * @param key The key of the item to retrieve.
   * @return The item, or null if the key is not found.
   */
  public ListItem get(Object key) {
    return getByKey(key);
  }

  /**
   * Returns the item at the specified index.
   *
   * @param index The index of the item to retrieve.
   * @return The item.
   * @throws IndexOutOfBoundsException If the index is out of range.
   */
  public ListItem getByIndex(int index) {
    verifyValidIndex(index, false);
    return (ListItem) items.values().toArray()[index];
  }

  /**
   * Returns unmodifiable list of items.
   *
   * @return The list of items.
   */
  public List<ListItem> getItems() {
    return Collections.unmodifiableList(new ArrayList<>(items.values()));
  }

  /**
   * Returns whether the list is empty.
   *
   * @return True if the list is empty, false otherwise.
   */
  public boolean isEmpty() {
    return items.isEmpty();
  }

  /**
   * Returns whether the list contains an item with the specified key.
   *
   * @param key The key of the item to search for.
   * @return True if the key is found, false otherwise.
   */
  public boolean containsKey(Object key) {
    return items.containsKey(key);
  }

  /**
   * Returns whether the list contains the specified item.
   *
   * @param item The item to search for.
   * @return True if the item is found, false otherwise.
   */
  public boolean contains(ListItem item) {
    return items.containsValue(item);
  }

  /**
   * Returns the total count of items in the list.
   *
   * @return The number of items in the list.
   */
  public int size() {
    return items.size();
  }

  /**
   * Returns an iterator over the entries in the list.
   *
   * @return An iterator over the entries.
   */
  public Iterator<ListItem> iterator() {
    return Collections.unmodifiableCollection(items.values()).iterator();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T select(ListItem item) {
    verifyItemInList(item);

    getInternalSelectedList().clear();
    getInternalSelectedList().add(item);

    ListBehavior list = inferList();

    if (list != null) {
      try {
        int index = indexOf(item.getKey());
        list.selectIndex(index);

      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T selectKey(Object key) {
    verifyItemInList(key);

    ListItem item = getByKey(key);
    return select(item);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T selectIndex(int index) {
    verifyValidIndex(index, false);

    ListItem item = getByIndex(index);
    return select(item);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListItem getSelected() {
    int index = getSelectedIndex();
    if (index == -1) {
      return null;
    }

    return getByIndex(index);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Object getSelectedKey() {
    ListItem item = getSelected();
    if (item == null) {
      return null;
    }

    return item.getKey();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getSelectedIndex() {
    ListBehavior list = inferList();

    if (list != null) {
      try {
        return list.getSelectedIndex();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    if (!getInternalSelectedList().isEmpty()) {
      return indexOf(getInternalSelectedList().get(0));
    }

    return -1;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public T setExpanse(Expanse expanse) {
    setComponentExpanse(expanse);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Expanse getExpanse() {
    return getComponentExpanse();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setLabel(String label) {
    this.label = label;
    setUnrestrictedProperty("label", this.label);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getLabel() {
    return this.label;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public T setHorizontalAlignment(Alignment alignment) {
    setComponentHorizontalAlignment(alignment);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Alignment getHorizontalAlignment() {
    return getComponentHorizontalAlignment();
  }

  /**
   * Adds a {@link ListSelectEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<ListSelectEvent> addSelectListener(
      EventListener<ListSelectEvent> listener) {
    return selectEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Adds a {@link ListSelectEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<ListSelectEvent> onSelect(EventListener<ListSelectEvent> listener) {
    return addSelectListener(listener);
  }

  /**
   * Gets the list of selected items.
   *
   * @return the selectedItems
   */
  protected List<ListItem> getInternalSelectedList() {
    return selectedItems;
  }

  /**
   * Verifies that the item is not null and is not already in the list.
   *
   * @param item The item to be verified.
   *
   * @throws NullPointerException If the item is null.
   * @throws IllegalArgumentException If the item is already in the list.
   */
  protected void verifyItemNotInList(ListItem item) {
    if (item == null) {
      throw new NullPointerException(NULL_ITEM_MESSAGE);
    }

    if (contains(item)) {
      throw new IllegalArgumentException(DUPLICATION_ITEM_MESSAGE);
    }

    if (containsKey(item.getKey())) {
      throw new IllegalArgumentException(DUPLICATION_ITEM_MESSAGE);
    }
  }

  /**
   * Verifies that the item is in the list.
   *
   * @param item The item to be verified.
   *
   * @throws IllegalArgumentException If the item is not in the list.
   */
  protected void verifyItemInList(ListItem item) {
    if (!contains(item)) {
      throw new IllegalArgumentException(ITEM_MISSING_MESSAGE);
    }
  }

  /**
   * Verifies that the key is in the list.
   *
   * @param key The key to be verified.
   *
   * @throws IllegalArgumentException If the key is not in the list.
   */
  protected void verifyItemInList(Object key) {
    if (!containsKey(key)) {
      throw new IllegalArgumentException(ITEM_MISSING_MESSAGE);
    }
  }

  /**
   * Verifies that the given index is not out of range.
   *
   * @param index The index to be verified.
   * @param insert Whether the index is for an insert operation.
   *
   * @throws IndexOutOfBoundsException If the index is out of range.
   */
  protected void verifyValidIndex(int index, boolean insert) {
    if (index < 0 || index >= items.size() + (insert ? 1 : 0)) {
      throw new IndexOutOfBoundsException(String.format(INVALID_INDEX_MESSAGE, index));
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void attachControlCallbacks() {
    super.attachControlCallbacks();
    selectEventSinkListenerRegistry.attach();
  }

  /**
   * Catches up with the changes made to the items list.
   */
  protected void catchupItemsChanges() {
    if (size() > 0) {
      doBulkInsert(0, items.values().stream().toList());
    }
  }

  /**
   * Catches up with the changes made to the selected items list.
   */
  protected void catchupSingleSelection() {
    if (getInternalSelectedList().size() == 1) {
      select(getInternalSelectedList().get(0));
    }
  }

  private ListBehavior inferList() {
    try {
      return (ListBehavior) ComponentAccessor.getDefault().getControl(this);
    } catch (IllegalAccessException e) {
      throw new DwcjRuntimeException(e);
    }
  }

  /**
   * Inserts multiple items at the specified index using BBj control API to avoid the overhead of
   * adding the items.
   *
   * @param index The index at which the items should be inserted.
   * @param items The list containing the items to be inserted.
   */
  private void doBulkInsert(int index, List<ListItem> items) {
    ListBehavior list = inferList();

    if (list != null) {
      try {
        List<String> itemValues = items.stream().map(ListItem::getText).toList();
        list.insertItems(index, new BBjVector(itemValues));
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }
  }

  /**
   * An item change listener. When a property of a List Item changes, this listener will update the
   * list item in the client.
   *
   * @author Hyyan Abo Fakher
   * @since 23.05
   */
  private final class ListItemChangeListener implements PropertyChangeListener {

    /**
     * {@inheritDoc}
     */
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
      ListItem item = (ListItem) evt.getSource();
      int index = indexOf(item.getKey());

      if (index >= 0) {
        setTextAt(index, item.getText());
      }
    }

    /**
     * Sets the text of the item at the specified index.
     *
     * @param index The index of the item.
     * @param text The text to set.
     */
    private void setTextAt(int index, String text) {
      ListBehavior list = inferList();
      if (list instanceof BBjComboBox) {
        try {
          ((BBjComboBox) list).setTextAt(index, text);
        } catch (BBjException e) {
          throw new DwcjRuntimeException(e);
        }
      }
    }
  }
}
