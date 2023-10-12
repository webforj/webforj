package org.dwcj.concern;

import org.dwcj.component.Component;
import org.dwcj.component.ExpanseBase;

/**
 * An interface that enables modifying a component's expanse.
 *
 * <p>
 * This interface provides methods to set and retrieve the expanse of a component. Expanse typically
 * refers to a rendering size of the component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 * @param <V> the type of the expanse, typically an enum implementing ExpanseBase.
 *
 * @see ExpanseBase
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasExpanse<T extends Component, V extends Enum<V> & ExpanseBase> {

  /**
   * Sets the expanse of the component.
   *
   * @param expanse the expanse to set for the component
   * @return the component itself
   */
  public T setExpanse(V expanse);

  /**
   * Retrieves the expanse of the component.
   *
   * @return the expanse of the component, typically an enum value.
   */
  public V getExpanse();
}
