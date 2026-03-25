package com.webforj.data.selection;

import com.webforj.data.repository.CollectionRepository;
import com.webforj.data.repository.HasRepository;
import com.webforj.data.repository.Repository;
import com.webforj.data.selection.repository.MultipleSelectableRepository;
import java.util.ArrayList;
import java.util.List;

public class MultipleSelectableComponentMock implements HasRepository<String>,
    MultipleSelectableRepository<MultipleSelectableComponentMock, String> {

  private List<String> items = new ArrayList<>(List.of("item1", "item2", "item3"));
  private List<Object> selectedKeys = new ArrayList<>();
  private Repository<String> repository = new CollectionRepository<>(items);

  @Override
  public MultipleSelectableComponentMock selectKey(Object... keys) {
    for (Object key : keys) {
      if (key != null && !selectedKeys.contains(key)) {
        selectedKeys.add(key);
      }
    }
    return this;
  }

  @Override
  public MultipleSelectableComponentMock deselectKey(Object... keys) {
    for (Object key : keys) {
      selectedKeys.remove(key);
    }
    return this;
  }

  @Override
  public MultipleSelectableComponentMock deselectAll() {
    selectedKeys.clear();
    return this;
  }

  @Override
  public Object getSelectedKey() {
    return selectedKeys.isEmpty() ? null : selectedKeys.get(0);
  }

  @Override
  public List<Object> getSelectedKeys() {
    return new ArrayList<>(selectedKeys);
  }

  @Override
  public Repository<String> getRepository() {
    return repository;
  }

  @Override
  public MultipleSelectableComponentMock setRepository(Repository<String> repository) {
    this.repository = repository;
    items.clear();
    repository.findAll().forEach(items::add);
    return this;
  }
}
