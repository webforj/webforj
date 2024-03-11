package com.webforj.component.event.mocks;

import com.webforj.component.Component;
import com.webforj.component.window.Window;
import com.webforj.data.repository.CollectionRepository;
import com.webforj.data.repository.HasRepository;
import com.webforj.data.repository.Repository;
import com.webforj.data.selection.repository.MultipleSelectableRepository;
import java.util.ArrayList;
import java.util.List;

public class MultipleSelectableComponentMock extends Component implements HasRepository<String>,
    MultipleSelectableRepository<MultipleSelectableComponentMock, String> {

  private List<String> items = List.of("item1", "item2", "item3");
  private List<Integer> selectedIndices = new ArrayList<>();
  private Repository<String> repository = new CollectionRepository<>(items);

  @Override
  public MultipleSelectableComponentMock selectIndex(int... index) {
    for (int i : index) {
      if (i >= 0 && i < items.size()) {
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

  @Override
  protected void onCreate(Window window) {
    // pass
  }

  @Override
  protected void onDestroy() {
    // pass
  }
}
