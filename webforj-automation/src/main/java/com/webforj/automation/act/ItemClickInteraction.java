package com.webforj.automation.act;

import com.webforj.automation.act.exception.InteractionRejectedException;

/**
 * Clicks an item of a component the way the user does.
 *
 * @param <C> the contract of the components the interaction serves
 * @param <I> the type of the items the component holds
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public interface ItemClickInteraction<C, I> extends ComponentInteraction<C> {

  /**
   * Clicks the given item of the given component.
   *
   * @param component the component holding the item
   * @param item the item to click
   * @param options where and how the pointer clicked
   *
   * @throws InteractionRejectedException if the user cannot do that with the component
   */
  void click(C component, I item, MouseOptions options);
}
