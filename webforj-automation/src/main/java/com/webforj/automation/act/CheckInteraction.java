package com.webforj.automation.act;

import com.webforj.automation.act.exception.InteractionRejectedException;

/**
 * Checks and unchecks a component the way the user does.
 *
 * @param <C> the contract of the components the interaction serves
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public interface CheckInteraction<C> extends ComponentInteraction<C> {

  /**
   * Reports whether the given component is checked.
   *
   * @param component the component
   *
   * @return true when the component is checked
   */
  boolean isChecked(C component);

  /**
   * Checks the given component.
   *
   * @param component the component to check
   *
   * @throws InteractionRejectedException if the user cannot do that with the component
   */
  void check(C component);

  /**
   * Unchecks the given component.
   *
   * @param component the component to uncheck
   *
   * @throws InteractionRejectedException if the user cannot do that with the component
   */
  void uncheck(C component);
}
