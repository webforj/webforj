package com.webforj.data.selection;

/**
 * The base interface for components that support item selection by index, key, or item reference.
 *
 * @param <V> the type of the selected item
 *
 * @see SingleSelectable
 * @see MultipleSelectable
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public interface Selectable<V>
    extends IndexSingleSelectionAware, KeySingleSelectionAware, ItemSingleSelectionAware<V> {
}
