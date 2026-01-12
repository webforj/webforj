package com.webforj;

import com.webforj.environment.ObjectTable;
import com.webforj.event.page.PageEventOptions;
import com.webforj.utilities.Assets;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Manages view transitions coordination between server and client.
 *
 * <p>
 * This class handles the lifecycle of view transitions, including registration, callback execution,
 * and cleanup. It is used internally by {@link Page} and {@link ViewTransition}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.11
 */
final class ViewTransitionManager {
  private static final String ASSETS_LOADED_KEY = "webforj.view-transitions.assets-loaded";
  private final Page page;
  private final Map<String, ViewTransition> pendingTransitions = new ConcurrentHashMap<>();

  /**
   * Ensures view transition CSS and JS assets are loaded on the page.
   *
   * @param page the page instance
   */
  static void ensureAssetsLoaded(Page page) {
    if (!ObjectTable.contains(ASSETS_LOADED_KEY)) {
      String css = Assets.contentOf("META-INF/resources/webforj/view-transitions.min.css");
      page.addInlineStyleSheet(css);

      String js = Assets.contentOf("META-INF/resources/webforj/view-transitions.min.js");
      page.executeJsVoidAsync(js);

      ObjectTable.put(ASSETS_LOADED_KEY, true);
    }
  }

  /**
   * Creates a new ViewTransitionManager and initializes the view transitions infrastructure.
   *
   * @param page the page instance
   */
  ViewTransitionManager(Page page) {
    this.page = page;
    ensureAssetsLoaded(page);

    // Configure event options to extract action and transitionId from event.detail
    PageEventOptions options = new PageEventOptions();
    options.addData("action", "event.detail.action");
    options.addData("transitionId", "event.detail.transitionId");

    // Register event listener for view transition callbacks from client
    page.addEventListener("vt-event", event -> {
      Map<String, Object> data = event.getData();
      String action = (String) data.get("action");
      String transitionId = (String) data.get("transitionId");

      if (transitionId == null) {
        return;
      }

      switch (action) {
        case "prepare":
          executePrepare(transitionId);
          break;
        case "update":
          executeUpdate(transitionId);
          break;
        case "ready":
          executeReady(transitionId);
          break;
        case "complete":
          executeComplete(transitionId);
          break;
        default:
          break;
      }
    }, options);
  }

  /**
   * Registers a view transition for tracking.
   *
   * @param transitionId the unique transition ID
   * @param transition the transition instance
   */
  void register(String transitionId, ViewTransition transition) {
    pendingTransitions.put(transitionId, transition);
  }

  /**
   * Applies exit styles and signals client that old state can be captured.
   *
   * @param transitionId the transition ID
   */
  void executePrepare(String transitionId) {
    ViewTransition transition = pendingTransitions.get(transitionId);
    if (transition != null) {
      transition.applyExitStyles();
      page.executeJsVoidAsync(
          "window.__webforjViewTransitions?.resolvePrepare('" + transitionId + "');");
    }
  }

  /**
   * Executes the update callback for a view transition.
   *
   * @param transitionId the transition ID
   */
  void executeUpdate(String transitionId) {
    ViewTransition transition = pendingTransitions.get(transitionId);
    if (transition != null) {
      transition.executeUpdate(() -> {
        // Resolve the JS promise after the async update completes
        page.executeJsVoidAsync(
            "window.__webforjViewTransitions?.resolveUpdate('" + transitionId + "');");
      });
    }
  }

  /**
   * Executes the ready callback for a view transition.
   *
   * @param transitionId the transition ID
   */
  void executeReady(String transitionId) {
    ViewTransition transition = pendingTransitions.get(transitionId);
    if (transition != null) {
      transition.executeReady();
    }
  }

  /**
   * Completes a view transition and removes it from tracking.
   *
   * @param transitionId the transition ID
   */
  void executeComplete(String transitionId) {
    pendingTransitions.remove(transitionId);
  }
}
