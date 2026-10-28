package com.webforj.automation.act.concern;

import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.automation.act.DeselectInteraction;
import com.webforj.automation.act.SelectIndexInteraction;
import com.webforj.automation.act.SelectInteraction;
import com.webforj.automation.act.SelectKeyInteraction;
import com.webforj.component.Component;
import com.webforj.data.selection.IndexMultiSelectable;
import com.webforj.data.selection.IndexMultiSelectionAware;
import com.webforj.data.selection.IndexSingleSelectable;
import com.webforj.data.selection.IndexSingleSelectionAware;
import com.webforj.data.selection.ItemMultiSelectable;
import com.webforj.data.selection.ItemMultiSelectionAware;
import com.webforj.data.selection.ItemSingleSelectable;
import com.webforj.data.selection.ItemSingleSelectionAware;
import com.webforj.data.selection.KeyMultiSelectable;
import com.webforj.data.selection.KeyMultiSelectionAware;
import com.webforj.data.selection.KeySingleSelectable;
import com.webforj.data.selection.KeySingleSelectionAware;
import java.lang.reflect.Array;
import java.util.List;

/**
 * Selects by item, index or key in every component that implements the selection contracts of
 * webforJ, and deselects in one that selects several items. The component reports the selection the
 * way it does itself.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class SelectableInteraction extends AbstractComponentInteraction<Component>
    implements SelectInteraction<Component, Object>, SelectIndexInteraction<Component>,
    SelectKeyInteraction<Component>, DeselectInteraction<Component, Object> {

  /**
   * Creates the interaction.
   */
  public SelectableInteraction() {
    super(Component.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isSupported(Component component) {
    return component instanceof ItemSingleSelectable<?, ?>
        || component instanceof ItemMultiSelectable<?, ?>
        || component instanceof IndexSingleSelectable<?>
        || component instanceof IndexMultiSelectable<?>
        || component instanceof KeySingleSelectable<?>
        || component instanceof KeyMultiSelectable<?>;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void select(Component component, Object item) {
    switch (component) {
      case ItemSingleSelectable<?, ?> single -> asItemSelectable(single).select(item);
      case ItemMultiSelectable<?, ?> multi -> asItemMultiSelectable(multi).select(arrayOf(item));
      default -> throw reject(component, "does not select items");
    }

    if (!getSelectedItems(component).contains(item)) {
      throw reject(component, "does not contain the item '" + item + "'");
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void selectIndex(Component component, int index) {
    switch (component) {
      case IndexSingleSelectable<?> selectable -> selectable.selectIndex(index);
      case IndexMultiSelectable<?> selectable -> selectable.selectIndex(index);
      default -> throw reject(component, "does not select by index");
    }

    boolean selected = component instanceof IndexMultiSelectionAware aware
        ? aware.getSelectedIndices().contains(index)
        : ((IndexSingleSelectionAware) component).getSelectedIndex() == index;
    if (!selected) {
      throw reject(component, "has no item at index " + index);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void selectKey(Component component, Object key) {
    switch (component) {
      case KeySingleSelectable<?> selectable -> selectable.selectKey(key);
      case KeyMultiSelectable<?> selectable -> selectable.selectKey(key);
      default -> throw reject(component, "does not select by key");
    }

    boolean selected =
        component instanceof KeyMultiSelectionAware aware ? aware.getSelectedKeys().contains(key)
            : key.equals(((KeySingleSelectionAware) component).getSelectedKey());
    if (!selected) {
      throw reject(component, "has no item with the key '" + key + "'");
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void deselect(Component component, Object item) {
    if (!(component instanceof ItemMultiSelectable<?, ?> selectable)) {
      throw reject(component, "does not deselect items");
    }

    if (!getSelectedItems(component).contains(item)) {
      return;
    }

    asItemMultiSelectable(selectable).deselect(arrayOf(item));
  }

  private static List<Object> getSelectedItems(Component component) {
    if (component instanceof ItemMultiSelectionAware<?> aware) {
      return List.copyOf(aware.getSelectedItems());
    }

    if (component instanceof ItemSingleSelectionAware<?> aware && aware.getSelected() != null) {
      return List.of(aware.getSelected());
    }

    return List.of();
  }

  // The contract takes its items as an array of their type, which an array of the type of the
  // item satisfies.
  private static Object[] arrayOf(Object item) {
    Object[] items = (Object[]) Array.newInstance(item.getClass(), 1);
    items[0] = item;

    return items;
  }

  @SuppressWarnings("unchecked")
  private static ItemSingleSelectable<?, Object> asItemSelectable(
      ItemSingleSelectable<?, ?> selectable) {
    return (ItemSingleSelectable<?, Object>) selectable;
  }

  @SuppressWarnings("unchecked")
  private static ItemMultiSelectable<?, Object> asItemMultiSelectable(
      ItemMultiSelectable<?, ?> selectable) {
    return (ItemMultiSelectable<?, Object>) selectable;
  }
}
