package org.dwcj.concern;

import org.dwcj.component.Component;
import org.dwcj.component.ExpanseBase;

/**
 * Interface facilitates implementation of behaviors to modify a component's expanse.
 *
 * @param <T> the type of the component
 * @param <V> the type of the expanse
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasExpanse<T extends Component, V extends Enum<V> & ExpanseBase> {

  /**
   * Set the expanse of the component.
   *
   * @param expanse the expanse to set for the component
   * @return the component itself
   */
  public T setExpanse(V expanse);

  /**
   * Returns the expanse of the component.
   *
   * @return the expanse of the component
   */
  public V getExpanse();
}
