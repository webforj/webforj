package com.webforj.automation.act.exception;

import com.webforj.automation.find.ComponentTree;
import com.webforj.component.Component;

/**
 * Thrown when a user cannot do with a component what the test asks for. The message names the
 * component and the reason and prints the component tree.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class InteractionRejectedException extends IllegalStateException {
  private static final long serialVersionUID = 1L;

  /**
   * Creates the rejection of an interaction with the given component.
   *
   * @param component the component the user cannot interact with
   * @param reason why, completing the sentence "The component X ..."
   */
  public InteractionRejectedException(Component component, String reason) {
    super("The component " + component.getClass().getSimpleName() + " " + reason + "."
        + System.lineSeparator() + ComponentTree.renderAll());
  }
}
