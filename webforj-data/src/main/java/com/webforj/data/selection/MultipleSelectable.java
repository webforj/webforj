package com.webforj.data.selection;

/**
 * An interface for components that support multiple selection of items by index, key, or item
 *
 * <p>
 * This interface introduces methods and properties specific to managing multiple selections within
 * a component.
 * </p>
 *
 * @param <T> the type of the component.
 * @param <V> the type of the selected item
 *
 * @see SingleSelectable
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public interface MultipleSelectable<T, V> extends Selectable<V>, IndexMultiSelectable<T>,
    KeyMultiSelectable<T>, ItemMultiSelectable<T, V> {
}
