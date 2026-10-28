package com.webforj.automation.act.concern;

import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.automation.act.FocusInteraction;
import com.webforj.concern.HasFocus;

/**
 * Moves the focus onto every component that can hold it.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class FocusableInteraction extends AbstractComponentInteraction<HasFocus<?>>
    implements FocusInteraction<HasFocus<?>> {

  /**
   * Creates the interaction.
   */
  public FocusableInteraction() {
    super(HasFocus.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void focus(HasFocus<?> focusable) {
    focusable.focus();
  }
}
