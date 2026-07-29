package com.webforj.devtools.craftforj.inspector.contribution.state.listbox;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.list.ListBox;
import com.webforj.component.list.MultipleSelectableList;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for ListBox selection mode.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ListBoxSelectionModeContribution extends EnumConcernContribution<ListBox> {

  /**
   * Creates a new ListBoxSelectionModeContribution.
   */
  public ListBoxSelectionModeContribution() {
    super(ListBox.class, "SelectionMode", FeatureCategory.STATE);
    setGetter(ListBox::getSelectionMode);
    setSetter((c, v) -> c.setSelectionMode((MultipleSelectableList.SelectionMode) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return MultipleSelectableList.SelectionMode.class;
  }

}
