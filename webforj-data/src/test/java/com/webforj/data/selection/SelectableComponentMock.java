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
  private List<Integer> selectedIndices = new ArrayList<>();
  private Repository<String> repository = new CollectionRepository<>(items);

  @Override
  public SelectableComponentMock deselect() {
    selectedIndices.clear();
    return this;
  }

  @Override
  public SelectableComponentMock select(String item) {
    if (item != null) {
      int index = items.indexOf(item);
      if (index >= 0) {
        selectedIndices.clear();
        selectedIndices.add(index);
      }
    }
    return this;
  }

  @Override
  public SelectableComponentMock selectKey(Object key) {
    if (key != null) {
      String item = repository.find(key).orElse(null);
      return select(item);
    }
    return this;
  }

  @Override
  public SelectableComponentMock selectIndex(int index) {
    if (index >= 0 && index < items.size()) {
      selectedIndices.clear();
      selectedIndices.add(index);
    }

    return this;
  }

  @Override
  public int getSelectedIndex() {
    return selectedIndices.isEmpty() ? -1 : selectedIndices.get(0);
  }

  @Override
  public Object getSelectedKey() {
    int index = getSelectedIndex();
    if (index >= 0 && index < items.size()) {
      return repository.getKey(items.get(index));
    }
    return null;
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
