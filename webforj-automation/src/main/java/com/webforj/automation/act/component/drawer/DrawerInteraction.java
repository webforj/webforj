package com.webforj.automation.act.component.drawer;

import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.CloseInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.component.drawer.Drawer;

/**
 * Closes open drawers.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class DrawerInteraction extends AbstractComponentInteraction<Drawer>
    implements CloseInteraction<Drawer> {

  /**
   * Creates the interaction.
   */
  public DrawerInteraction() {
    super(Drawer.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void close(Drawer drawer) {
    if (!drawer.isOpened()) {
      return;
    }

    drawer.close();
  }
}
