package com.webforj.data.selection.event;

import com.webforj.component.Component;
import com.webforj.data.repository.HasRepository;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * A select event that emitted when an item inside a repository is selected.
 *
 * @param <T> the type of the component
 * @param <V> the type of the selected item
 *
 * @see AbstractSelectEvent
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class RepositorySelectEvent<T extends Component & HasRepository<V>, V>
    extends AbstractSelectEvent<T, V> {

  protected RepositorySelectEvent(T component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public V getSelectedItem() {
    int index = getSelectedIndex();
    if (index == -1) {
      return null;
    }

    HasRepository<V> component = (HasRepository<V>) getComponent();
    return component.getRepository().findByIndex(index).orElse(null);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<V> getSelectedItems() {
    List<Integer> indices = getSelectedIndices();

    if (indices.isEmpty()) {
      return Collections.emptyList();
    }

    HasRepository<V> component = (HasRepository<V>) getComponent();
    return indices.stream().map(index -> component.getRepository().findByIndex(index).orElse(null))
        .toList();
  }
}
