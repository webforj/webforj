package com.webforj.automation.act;

import com.webforj.automation.act.exception.InteractionRejectedException;

/**
 * Opens a component the way the user does.
 *
 * @param <C> the contract of the components the interaction serves
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public interface OpenInteraction<C> extends ComponentInteraction<C> {

  /**
   * Opens the given component.
   *
   * @param component the component to open
   *
   * @throws InteractionRejectedException if the user cannot do that with the component
   */
  void open(C component);
}
