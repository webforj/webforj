package com.webforj.component.event.mocks;

import com.webforj.component.Component;
import com.webforj.component.event.AbstractSelectEvent;
import com.webforj.data.repository.HasRepository;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class SelectEventMock<T extends Component & HasRepository<V>, V>
    extends AbstractSelectEvent<T, V> {

  public SelectEventMock(T component, Map<String, Object> eventMap) {
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
