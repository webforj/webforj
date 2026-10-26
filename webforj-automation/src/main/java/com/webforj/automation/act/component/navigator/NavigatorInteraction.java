package com.webforj.automation.act.component.navigator;

import com.webforj.automation.act.UserInteraction;
import com.webforj.component.navigator.Navigator;
import com.webforj.component.navigator.event.NavigatorMoveFirstEvent;
import com.webforj.component.navigator.event.NavigatorMoveLastEvent;
import com.webforj.component.navigator.event.NavigatorMoveNextEvent;
import com.webforj.component.navigator.event.NavigatorMovePreviousEvent;
import com.webforj.data.Paginator;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

/**
 * Moves navigators between their pages, staying on the first or the last page at the ends.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class NavigatorInteraction {

  private NavigatorInteraction() {}

  /**
   * Returns the interaction that moves a navigator to its first page.
   *
   * <pre>{@code
   * UserInteraction.perform(navigator, NavigatorInteraction.moveFirst())
   * }</pre>
   *
   * @return the interaction to perform on the navigator
   */
  public static Consumer<Navigator> moveFirst() {
    return navigator -> UserInteraction
        .fireEvent(new NavigatorMoveFirstEvent(navigator, createPageData(navigator, 1)));
  }

  /**
   * Returns the interaction that moves a navigator to its last page.
   *
   * @return the interaction to perform on the navigator
   */
  public static Consumer<Navigator> moveLast() {
    return navigator -> UserInteraction.fireEvent(
        new NavigatorMoveLastEvent(navigator, createPageData(navigator, getTotalPages(navigator))));
  }

  /**
   * Returns the interaction that moves a navigator to its next page.
   *
   * @return the interaction to perform on the navigator
   */
  public static Consumer<Navigator> moveNext() {
    return navigator -> {
      int target = Math.min(navigator.getPaginator().getCurrent() + 1, getTotalPages(navigator));
      UserInteraction
          .fireEvent(new NavigatorMoveNextEvent(navigator, createPageData(navigator, target)));
    };
  }

  /**
   * Returns the interaction that moves a navigator to its previous page.
   *
   * @return the interaction to perform on the navigator
   */
  public static Consumer<Navigator> movePrevious() {
    return navigator -> {
      int target = Math.max(navigator.getPaginator().getCurrent() - 1, 1);
      UserInteraction
          .fireEvent(new NavigatorMovePreviousEvent(navigator, createPageData(navigator, target)));
    };
  }

  private static int getTotalPages(Navigator navigator) {
    return Math.max(navigator.getPaginator().getTotalPages(), 1);
  }

  private static Map<String, Object> createPageData(Navigator navigator, int current) {
    Paginator paginator = navigator.getPaginator();
    Paginator target = new Paginator(paginator.getTotalItems(), paginator.getSize());
    target.setCurrent(current);

    Map<String, Object> data = new HashMap<>();
    data.put("current", target.getCurrent());
    data.put("startIndex", target.getStartIndex());
    data.put("endIndex", target.getEndIndex());

    return data;
  }
}
