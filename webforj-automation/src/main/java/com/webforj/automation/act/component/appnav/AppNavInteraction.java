package com.webforj.automation.act.component.appnav;

import com.webforj.automation.act.UserInteraction;
import com.webforj.component.layout.appnav.AppNav;
import com.webforj.component.layout.appnav.event.AppNavSearchEvent;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

/**
 * Searches the items of navigations.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class AppNavInteraction {

  private AppNavInteraction() {}

  /**
   * Returns the interaction that searches the items of a navigation for the given term, the way the
   * user types into its search field. The navigation holds the term, then reports it.
   *
   * <pre>{@code
   * UserInteraction.perform(nav, AppNavInteraction.search("inv"))
   * }</pre>
   *
   * @param term the term the user entered
   *
   * @return the interaction to perform on the navigation
   */
  public static Consumer<AppNav> search(String term) {
    return nav -> {
      nav.getSearch().setTerm(term);

      Map<String, Object> data = new HashMap<>();
      data.put("term", term);
      UserInteraction.fireEvent(new AppNavSearchEvent(nav, data));
    };
  }
}
