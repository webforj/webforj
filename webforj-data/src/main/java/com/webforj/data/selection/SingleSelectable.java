package com.webforj.data.selection;

/**
 * An interface for components that support single item selection by index, key, or item reference.
 *
 * <p>
 * This interface provides methods and properties for selecting and retrieving the currently
 * selected item within a component.
 * </p>
 *
 * @param <T> the type of the component
 * @param <V> the type of the selected item
 *
 * @see MultipleSelectable
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public interface SingleSelectable<T, V> extends Selectable<V>, IndexSingleSelectable<T>,
    KeySingleSelectable<T>, ItemSingleSelectable<T, V> {
}
