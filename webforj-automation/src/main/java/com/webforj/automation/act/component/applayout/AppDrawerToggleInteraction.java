package com.webforj.automation.act.component.applayout;

import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.ClickInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.automation.act.MouseOptions;
import com.webforj.automation.find.ComponentTree;
import com.webforj.component.Component;
import com.webforj.component.layout.applayout.AppDrawerToggle;
import com.webforj.component.layout.applayout.AppLayout;

/**
 * Clicks drawer toggles, which opens the closed drawer and closes the open drawer of every app
 * layout of the page.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class AppDrawerToggleInteraction extends AbstractComponentInteraction<AppDrawerToggle>
    implements ClickInteraction<AppDrawerToggle> {

  /**
   * Creates the interaction.
   */
  public AppDrawerToggleInteraction() {
    super(AppDrawerToggle.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void click(AppDrawerToggle toggle, MouseOptions options) {
    for (Component root : ComponentTree.getRoots()) {
      ComponentTree.walk(root, component -> {
        if (component instanceof AppLayout layout) {
          layout.setDrawerOpened(!layout.isDrawerOpened());
        }
      });
    }
  }
}
