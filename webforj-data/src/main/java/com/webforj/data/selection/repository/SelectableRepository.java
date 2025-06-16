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
@SuppressWarnings("unused")
public interface SelectableRepository<T extends HasRepository<V>, V> extends Selectable<V> {

  /**
   * {@inheritDoc}
   */
  @Override
  default V getSelected() {
    HasRepository<V> self = (HasRepository<V>) this;
    return self.getRepository().findByIndex(getSelectedIndex()).orElse(null);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  default Object getSelectedKey() {
    V selected = getSelected();
    if (selected == null) {
      return null;
    }

    HasRepository<V> self = (HasRepository<V>) this;
    return self.getRepository().getKey(selected);
  }
}
