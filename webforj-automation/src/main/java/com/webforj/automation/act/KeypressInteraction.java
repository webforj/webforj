package com.webforj.automation.act;

import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.event.KeypressEvent;

/**
 * Presses a key on a component the way the user does.
 *
 * @param <C> the contract of the components the interaction serves
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public interface KeypressInteraction<C> extends ComponentInteraction<C> {

  /**
   * Presses the given key while the given component has the focus.
   *
   * @param component the component to press the key on
   * @param key the key to press
   * @param options the modifier keys held
   *
   * @throws InteractionRejectedException if the user cannot do that with the component
   */
  void keypress(C component, KeypressEvent.Key key, KeyOptions options);
}
