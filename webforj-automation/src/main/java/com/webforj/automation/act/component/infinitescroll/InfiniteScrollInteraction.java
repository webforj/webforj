package com.webforj.automation.act.component.infinitescroll;

import com.webforj.automation.act.UserInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.infinitescroll.InfiniteScroll;
import com.webforj.component.infinitescroll.event.InfiniteScrollEvent;
import java.util.HashMap;
import java.util.function.Consumer;

/**
 * Scrolls infinite scrolls to their end until they are completed.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class InfiniteScrollInteraction {

  private InfiniteScrollInteraction() {}

  /**
   * Returns the interaction that scrolls an infinite scroll to its end, which asks the application
   * for more.
   *
   * <pre>{@code
   * UserInteraction.perform(infiniteScroll, InfiniteScrollInteraction.scroll())
   * }</pre>
   *
   * @return the interaction to perform on the infinite scroll
   * @throws InteractionRejectedException when performed, if the infinite scroll is completed
   */
  public static Consumer<InfiniteScroll> scroll() {
    return infiniteScroll -> {
      if (infiniteScroll.isCompleted()) {
        throw new InteractionRejectedException(infiniteScroll,
            "is completed and asks for no more when a user scrolls");
      }

      UserInteraction.fireEvent(new InfiniteScrollEvent(infiniteScroll, new HashMap<>()));
    };
  }
}
