package com.webforj.automation.act.component.refresher;

import com.webforj.automation.act.UserInteraction;
import com.webforj.component.refresher.Refresher;
import com.webforj.component.refresher.event.RefresherRefreshEvent;
import java.util.HashMap;
import java.util.function.Consumer;

/**
 * Pulls refreshers until they refresh.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class RefresherInteraction {

  private RefresherInteraction() {}

  /**
   * Returns the interaction that pulls a refresher until it refreshes.
   *
   * <pre>{@code
   * UserInteraction.perform(refresher, RefresherInteraction.refresh())
   * }</pre>
   *
   * @return the interaction to perform on the refresher
   */
  public static Consumer<Refresher> refresh() {
    return refresher -> UserInteraction
        .fireEvent(new RefresherRefreshEvent(refresher, new HashMap<>()));
  }
}
