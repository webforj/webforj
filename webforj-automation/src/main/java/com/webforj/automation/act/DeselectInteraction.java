package com.webforj.automation.act;

import com.webforj.automation.act.exception.InteractionRejectedException;

/**
 * Deselects an item of a component the way the user does.
 *
 * @param <C> the contract of the components the interaction serves
 * @param <I> the type of the items the component holds
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public interface DeselectInteraction<C, I> extends ComponentInteraction<C> {

  /**
   * Deselects the given item in the given component.
   *
   * @param component the component to deselect in
   * @param item the item to deselect
   *
   * @throws InteractionRejectedException if the user cannot do that with the component
   */
  void deselect(C component, I item);
}
