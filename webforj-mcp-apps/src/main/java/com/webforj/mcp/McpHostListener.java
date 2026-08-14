package com.webforj.mcp;

import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.Page;
import com.webforj.annotation.AppListenerPriority;
import com.webforj.event.page.PageEventOptions;

/**
 * Connects an embedded application to its {@link McpHost}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AppListenerPriority(0)
public class McpHostListener implements AppLifecycleListener {

  private static final String MESSAGE_EVENT = "webforj-mcp-message";

  /**
   * {@inheritDoc}
   */
  @Override
  public void onWillRun(App app) {
    if (!Page.isPresent() || !Page.getCurrent().isEmbedded()) {
      return;
    }

    Page page = Page.getCurrent();
    McpHost host = new McpHost(page);

    // The application renders in its own document, which no style of the embedding page reaches.
    // The window center scrolls, so a frame smaller than the view never clips it unreachably.
    page.addInlineStyleSheet("dwc-window-center { overflow-y: scroll !important; }", false);

    PageEventOptions options = new PageEventOptions();
    options.addData("payload", "event.detail");
    page.addEventListener(MESSAGE_EVENT, event -> {
      Object payload = event.getData().get("payload");
      if (payload != null) {
        host.dispatch(payload.toString());
      }
    }, options);
  }

  @Override
  public void onDidRun(App app) {
    McpHost.ifPresent(McpHost::ready);
  }

  @Override
  public void onWillTerminate(App app) {
    McpHost.ifPresent(McpHost::destroy);
  }
}
