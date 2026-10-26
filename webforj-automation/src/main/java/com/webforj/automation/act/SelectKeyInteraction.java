package com.webforj.automation.act;

import com.webforj.automation.act.exception.InteractionRejectedException;

/**
 * Selects the item with a key of a component the way the user does.
 *
 * @param <C> the contract of the components the interaction serves
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public interface SelectKeyInteraction<C> extends ComponentInteraction<C> {

  /**
   * Selects the item with the given key in the given component.
   *
   * @param component the component to select in
   * @param key the key of the item to select
   *
   * @throws InteractionRejectedException if the user cannot do that with the component
   */
  void selectKey(C component, Object key);
}
