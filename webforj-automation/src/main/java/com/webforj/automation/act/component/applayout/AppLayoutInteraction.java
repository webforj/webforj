package com.webforj.automation.act.component.applayout;

import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.CloseInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.automation.act.OpenInteraction;
import com.webforj.component.layout.applayout.AppLayout;

/**
 * Opens and closes the drawer of app layouts.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class AppLayoutInteraction extends AbstractComponentInteraction<AppLayout>
    implements OpenInteraction<AppLayout>, CloseInteraction<AppLayout> {

  /**
   * Creates the interaction.
   */
  public AppLayoutInteraction() {
    super(AppLayout.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void open(AppLayout layout) {
    layout.setDrawerOpened(true);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void close(AppLayout layout) {
    layout.setDrawerOpened(false);
  }
}
