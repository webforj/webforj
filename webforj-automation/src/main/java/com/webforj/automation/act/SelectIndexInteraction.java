package com.webforj.automation.act;

import com.webforj.automation.act.exception.InteractionRejectedException;

/**
 * Selects the item at an index of a component the way the user does.
 *
 * @param <C> the contract of the components the interaction serves
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public interface SelectIndexInteraction<C> extends ComponentInteraction<C> {

  /**
   * Selects the item at the given index in the given component.
   *
   * @param component the component to select in
   * @param index the index to select
   *
   * @throws InteractionRejectedException if the user cannot do that with the component
   */
  void selectIndex(C component, int index);
}
