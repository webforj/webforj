package org.dwcj.data.selection.repository;

import org.dwcj.component.Component;
import org.dwcj.data.repository.HasRepository;
import org.dwcj.data.selection.SingleSelectable;

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
public interface SingleSelectableRepository<T extends Component & HasRepository<V>, V>
    extends SelectableRepository<T, V>, SingleSelectable<T, V> {

  /**
   * {@inheritDoc}
   */
  @Override
  default T select(V item) {
    HasRepository<V> self = (HasRepository<V>) this;
    int index = self.getRepository().getIndex(item);

    return selectIndex(index);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  default T selectKey(Object key) {
    HasRepository<V> self = (HasRepository<V>) this;
    V item = self.getRepository().find(key).orElse(null);

    return select(item);
  }
}
