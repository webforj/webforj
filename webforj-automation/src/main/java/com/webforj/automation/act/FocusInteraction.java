package com.webforj.automation.act;

import com.webforj.automation.act.exception.InteractionRejectedException;

/**
 * Moves the focus onto a component the way the user does.
 *
 * @param <C> the contract of the components the interaction serves
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public interface FocusInteraction<C> extends ComponentInteraction<C> {

  /**
   * Moves the focus onto the given component.
   *
   * @param component the component to focus
   *
   * @throws InteractionRejectedException if the user cannot do that with the component
   */
  void focus(C component);
}
