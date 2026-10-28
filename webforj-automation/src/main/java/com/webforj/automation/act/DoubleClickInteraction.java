package com.webforj.automation.act;

import com.webforj.automation.act.exception.InteractionRejectedException;

/**
 * Double clicks an item of a component the way the user does.
 *
 * @param <C> the contract of the components the interaction serves
 * @param <I> the type of the items the component holds
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public interface DoubleClickInteraction<C, I> extends ComponentInteraction<C> {

  /**
   * Double clicks the given item of the given component.
   *
   * @param component the component holding the item
   * @param item the item to double click
   * @param options where and how the pointer clicked
   *
   * @throws InteractionRejectedException if the user cannot do that with the component
   */
  void doubleClick(C component, I item, MouseOptions options);
}
