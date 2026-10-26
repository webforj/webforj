package com.webforj.automation.act;

import com.webforj.automation.act.exception.InteractionRejectedException;

/**
 * Types text into a component the way the user does.
 *
 * @param <C> the contract of the components the interaction serves
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public interface TypeInteraction<C> extends ComponentInteraction<C> {

  /**
   * Types the given text into the given component, replacing its current text.
   *
   * @param component the component to type into
   * @param text the text to type
   *
   * @throws InteractionRejectedException if the user cannot do that with the component
   */
  void type(C component, String text);
}
