package org.dwcj.data.selection;

import java.util.ArrayList;
import java.util.List;
import org.dwcj.component.Component;
import org.dwcj.component.window.Window;
import org.dwcj.data.repository.CollectionRepository;
import org.dwcj.data.repository.HasRepository;
import org.dwcj.data.repository.Repository;
import org.dwcj.data.selection.repository.SingleSelectableRepository;

class SelectableComponentMock extends Component
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
  public SelectableComponentMock selectIndex(int index) {
    if (index >= 0 && index < items.size()) {
      selectedIndices.add(index);
    }

    return this;
  }

  @Override
  public int getSelectedIndex() {
    return selectedIndices.isEmpty() ? -1 : selectedIndices.get(0);
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

  @Override
  protected void onCreate(Window window) {
    // pass
  }

  @Override
  protected void onDestroy() {
    // pass
  }
}
