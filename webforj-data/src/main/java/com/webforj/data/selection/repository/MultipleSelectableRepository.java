package com.webforj.data.selection.repository;

import com.webforj.data.repository.HasRepository;
import com.webforj.data.selection.ItemMultiSelectable;
import com.webforj.data.selection.KeyMultiSelectable;
import java.util.List;

/**
 * An interface for components that support multiple selection and they provide a repository.
 *
 * <p>
 * This interface introduces methods and properties specific to managing multiple selections within
 * a component.
 * </p>
 *
 * @param <T> the type of the component.
 * @param <V> the type of the selected item
 *
 * @see SingleSelectableRepository
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public interface MultipleSelectableRepository<T extends HasRepository<V>, V>
    extends SelectableRepository<T, V>, KeyMultiSelectable<T>, ItemMultiSelectable<T, V> {

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings("unchecked")
  default T select(V... items) {
    Object[] keys = new Object[items.length];
    HasRepository<V> self = (HasRepository<V>) this;

    for (int i = 0; i < items.length; i++) {
      if (items[i] != null) {
        keys[i] = self.getRepository().getKey(items[i]);
      }
    }

    return selectKey(keys);
  }


  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings("unchecked")
  default T deselect(V... items) {
    Object[] keys = new Object[items.length];
    HasRepository<V> self = (HasRepository<V>) this;

    for (int i = 0; i < items.length; i++) {
      if (items[i] != null) {
        keys[i] = self.getRepository().getKey(items[i]);
      }
    }

    return deselectKey(keys);
  }


  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings({"unchecked"})
  default List<V> getSelectedItems() {
    HasRepository<V> self = (HasRepository<V>) this;
    List<Object> keys = getSelectedKeys();

    return keys.stream().map(key -> self.getRepository().find(key).orElse(null))
        .filter(item -> item != null).toList();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings({"unchecked"})
  default List<Object> getSelectedKeys() {
    HasRepository<V> self = (HasRepository<V>) this;
    List<V> items = getSelectedItems();

    return items.stream().map(self.getRepository()::getKey).toList();
  }

}
