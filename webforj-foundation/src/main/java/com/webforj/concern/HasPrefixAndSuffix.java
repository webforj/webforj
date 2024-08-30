package com.webforj.concern;

import com.webforj.component.Component;

/**
 * An interface for modifying a component's prefix and suffix components.
 *
 * <p>
 * This interface provides methods to set and retrieve the prefix and suffix components for the
 * component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @see HasPrefix
 * @see HasSuffix
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public interface HasPrefixAndSuffix<T extends Component> extends HasPrefix<T>, HasSuffix<T> {
}
