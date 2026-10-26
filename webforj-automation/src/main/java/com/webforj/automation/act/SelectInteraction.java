package com.webforj.automation.act;

import com.webforj.automation.act.exception.InteractionRejectedException;

/**
 * Selects an item of a component the way the user does.
 *
 * @param <C> the contract of the components the interaction serves
 * @param <I> the type of the items the component holds
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public interface SelectInteraction<C, I> extends ComponentInteraction<C> {

  /**
   * Selects the given item in the given component.
   *
   * @param component the component to select in
   * @param item the item to select
   *
   * @throws InteractionRejectedException if the user cannot do that with the component
   */
  void select(C component, I item);
}
