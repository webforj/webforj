package com.webforj.data.selection.repository;

import com.webforj.data.repository.HasRepository;
import com.webforj.data.selection.Selectable;

/**
 * The base interface for repositories that support selection.
 *
 * @param <T> the type of the repository
 * @param <V> the type of the selected item
 *
 * @see SingleSelectableRepository
 * @see MultipleSelectableRepository
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public interface SelectableRepository<T extends HasRepository<V>, V> extends Selectable<V> {

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings({"unchecked"})
  default V getSelected() {
    Object key = getSelectedKey();
    if (key == null) {
      return null;
    }

    HasRepository<V> self = (HasRepository<V>) this;
    return self.getRepository().find(key).orElse(null);
  }
}
