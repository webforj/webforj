package com.webforj.data.selection.repository;

import com.webforj.data.repository.HasRepository;
import com.webforj.data.selection.SingleSelectable;

/**
 * An interface for components that support single item selection and they provide a repository.
 *
 * <p>
 * This interface provides methods and properties for selecting and retrieving the currently
 * selected item within a component.
 * </p>
 *
 * @param <T> the type of the component
 * @param <V> the type of the selected item
 *
 * @see MultipleSelectableRepository
 * @see SingleSelectable
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public interface SingleSelectableRepository<T extends HasRepository<V>, V>
    extends SelectableRepository<T, V>, SingleSelectable<T, V> {

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings({"unchecked"})
  default T select(V item) {
    if (item == null) {
      return selectKey(null);
    }

    HasRepository<V> self = (HasRepository<V>) this;
    Object key = self.getRepository().getKey(item);

    return selectKey(key);
  }

}
