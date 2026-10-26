package com.webforj.automation.act.component.dialog;

import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.CloseInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.component.dialog.Dialog;

/**
 * Closes open dialogs with the Escape key or a click outside of them.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class DialogInteraction extends AbstractComponentInteraction<Dialog>
    implements CloseInteraction<Dialog> {

  /**
   * Creates the interaction.
   */
  public DialogInteraction() {
    super(Dialog.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void close(Dialog dialog) {
    if (!dialog.isOpened()) {
      return;
    }

    if (!dialog.isCancelOnEscKey() && !dialog.isCancelOnOutsideClick()) {
      throw reject(dialog, "closes neither on the Escape key nor on a click outside of it and a "
          + "user cannot close it");
    }

    dialog.close();
  }
}
