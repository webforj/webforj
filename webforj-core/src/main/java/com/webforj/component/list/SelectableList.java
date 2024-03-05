package com.webforj.component.list;

import com.webforj.component.Component;
import com.webforj.data.selection.SingleSelectable;

/**
 * An interface for lists that support single item selection.
 *
 * <p>
 * This interface provides methods and properties for selecting and retrieving the currently
 * selected item within a list.
 * </p>
 *
 * @param <T> the type of the component
 *
 * @see MultipleSelectableList
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface SelectableList<T extends Component> extends SingleSelectable<T, ListItem> {

}
