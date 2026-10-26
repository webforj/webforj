package com.webforj.automation.act;

import com.webforj.automation.act.exception.InteractionRejectedException;

/**
 * Closes a component the way the user does.
 *
 * @param <C> the contract of the components the interaction serves
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public interface CloseInteraction<C> extends ComponentInteraction<C> {

  /**
   * Closes the given component.
   *
   * @param component the component to close
   *
   * @throws InteractionRejectedException if the user cannot do that with the component
   */
  void close(C component);
}
