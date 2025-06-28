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
  private List<Integer> selectedIndices = new ArrayList<>();
  private Repository<String> repository = new CollectionRepository<>(items);

  @Override
  public MultipleSelectableComponentMock select(String... selectedItems) {
    for (String item : selectedItems) {
      if (item != null) {
        int index = items.indexOf(item);
        if (index >= 0 && !selectedIndices.contains(index)) {
          selectedIndices.add(index);
        }
      }
    }
    return this;
  }

  @Override
  public MultipleSelectableComponentMock selectKey(Object... keys) {
    for (Object key : keys) {
      if (key != null) {
        String item = repository.find(key).orElse(null);
        if (item != null) {
          int index = items.indexOf(item);
          if (index >= 0 && !selectedIndices.contains(index)) {
            selectedIndices.add(index);
          }
        }
      }
    }
    return this;
  }

  @Override
  public MultipleSelectableComponentMock selectIndex(int... index) {
    for (int i : index) {
      if (i >= 0 && i < items.size() && !selectedIndices.contains(i)) {
        selectedIndices.add(i);
      }
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
  public List<Object> getSelectedKeys() {
    List<Object> keys = new ArrayList<>();
    for (int index : selectedIndices) {
      if (index >= 0 && index < items.size()) {
        keys.add(repository.getKey(items.get(index)));
      }
    }
    return keys;
  }

  @Override
  public MultipleSelectableComponentMock deselect(String... itemsToDeselect) {
    for (String item : itemsToDeselect) {
      if (item != null) {
        int index = items.indexOf(item);
        if (index >= 0) {
          selectedIndices.remove(Integer.valueOf(index));
        }
      }
    }
    return this;
  }

  @Override
  public MultipleSelectableComponentMock deselectKey(Object... keys) {
    for (Object key : keys) {
      if (key != null) {
        String item = repository.find(key).orElse(null);
        if (item != null) {
          int index = items.indexOf(item);
          if (index >= 0) {
            selectedIndices.remove(Integer.valueOf(index));
          }
        }
      }
    }
    return this;
  }

  @Override
  public MultipleSelectableComponentMock deselectIndex(int... index) {
    for (int i : index) {
      if (i >= 0 && i < items.size()) {
        selectedIndices.remove(Integer.valueOf(i));
      }
    }

    return this;
  }

  @Override
  public MultipleSelectableComponentMock deselectAll() {
    selectedIndices.clear();
    return this;
  }

  @Override
  public List<Integer> getSelectedIndices() {
    return new ArrayList<>(selectedIndices);
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
