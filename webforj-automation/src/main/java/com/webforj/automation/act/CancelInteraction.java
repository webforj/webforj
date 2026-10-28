package com.webforj.automation.act;

import com.webforj.automation.act.exception.InteractionRejectedException;

/**
 * Cancels what a component is doing the way the user does.
 *
 * @param <C> the contract of the components the interaction serves
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public interface CancelInteraction<C> extends ComponentInteraction<C> {

  /**
   * Cancels the given component.
   *
   * @param component the component to cancel
   *
   * @throws InteractionRejectedException if the user cannot do that with the component
   */
  void cancel(C component);
}
