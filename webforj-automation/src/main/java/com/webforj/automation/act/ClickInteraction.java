package com.webforj.automation.act;

import com.webforj.automation.act.exception.InteractionRejectedException;

/**
 * Clicks a component the way the user does.
 *
 * @param <C> the contract of the components the interaction serves
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public interface ClickInteraction<C> extends ComponentInteraction<C> {

  /**
   * Clicks the given component.
   *
   * @param component the component to click
   * @param options where and how the pointer clicked
   *
   * @throws InteractionRejectedException if the user cannot do that with the component
   */
  void click(C component, MouseOptions options);
}
