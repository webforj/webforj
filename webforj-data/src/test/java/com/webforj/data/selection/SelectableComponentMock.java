package com.webforj.data.selection;

import com.webforj.data.repository.CollectionRepository;
import com.webforj.data.repository.HasRepository;
import com.webforj.data.repository.Repository;
import com.webforj.data.selection.repository.SingleSelectableRepository;
import java.util.ArrayList;
import java.util.List;

class SelectableComponentMock
    implements HasRepository<String>, SingleSelectableRepository<SelectableComponentMock, String> {

  private List<String> items = new ArrayList<>(List.of("item1", "item2", "item3"));
  private Repository<String> repository = new CollectionRepository<>(items);
  private String selectedKey = null;

  @Override
  public SelectableComponentMock deselect() {
    selectedKey = null;
    return this;
  }

  @Override
  public SelectableComponentMock select(String item) {
    selectedKey = item != null && items.contains(item) ? item : selectedKey;
    return this;
  }

  @Override
  public SelectableComponentMock selectKey(Object key) {
    return select(repository.find(key).orElse(null));
  }

  @Override
  public Object getSelectedKey() {
    return selectedKey;
  }

  @Override
  public Repository<String> getRepository() {
    return repository;
  }

  @Override
  public SelectableComponentMock setRepository(Repository<String> repository) {
    this.repository = repository;
    items.clear();
    repository.findAll().forEach(items::add);
    return this;
  }
}
