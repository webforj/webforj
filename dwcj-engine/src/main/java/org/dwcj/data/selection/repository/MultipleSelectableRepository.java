package org.dwcj.data.selection.repository;

import java.util.List;
import org.dwcj.component.Component;
import org.dwcj.data.repository.HasRepository;
import org.dwcj.data.selection.MultipleSelectable;
import org.dwcj.data.selection.SingleSelectable;

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
 * @see SingleSelectable
 * @see MultipleSelectable
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public interface MultipleSelectableRepository<T extends Component & HasRepository<V>, V>
    extends SelectableRepository<T, V>, MultipleSelectable<T, V> {

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings("unchecked")
  default T select(V... items) {
    HasRepository<V> self = (HasRepository<V>) this;

    for (V i : items) {
      int index = self.getRepository().getIndex(i);
      selectIndex(index);
    }

    return (T) this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings("unchecked")
  default T selectKey(Object... keys) {
    HasRepository<V> self = (HasRepository<V>) this;

    for (Object k : keys) {
      V item = self.getRepository().find(k).orElse(null);
      select(item);
    }

    return (T) this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings("unchecked")
  default T deselect(V... item) {
    HasRepository<V> self = (HasRepository<V>) this;

    for (V i : item) {
      int index = self.getRepository().getIndex(i);
      deselectIndex(index);
    }

    return (T) this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings("unchecked")
  default T deselectKey(Object... key) {
    HasRepository<V> self = (HasRepository<V>) this;

    for (Object k : key) {
      V item = self.getRepository().find(k).orElse(null);
      deselect(item);
    }

    return (T) this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  default List<V> getSelectedItems() {
    HasRepository<V> self = (HasRepository<V>) this;
    List<Integer> selected = getSelectedIndices();

    return selected.stream().map(index -> self.getRepository().findByIndex(index).orElse(null))
        .toList();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  default List<Object> getSelectedKeys() {
    HasRepository<V> self = (HasRepository<V>) this;
    List<V> items = getSelectedItems();

    return items.stream().map(self.getRepository()::getKey).toList();
  }

  List<Integer> getSelectedIndices();
}
