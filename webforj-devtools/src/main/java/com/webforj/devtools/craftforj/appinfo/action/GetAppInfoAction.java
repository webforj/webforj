package com.webforj.devtools.craftforj.appinfo.action;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.appinfo.AppInfoCollector;
import com.webforj.devtools.craftforj.appinfo.model.AppInfo;

/**
 * Action handler that returns environment information about the running application.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class GetAppInfoAction implements CraftforjActionHandler<AppInfo> {

  /**
   * The action name for this handler.
   */
  public static final String ACTION = "appinfo.getAppInfo";

  private final AppInfoCollector collector;

  /**
   * Creates a new GetAppInfoAction.
   *
   * @param collector the collector used to gather the information
   */
  public GetAppInfoAction(AppInfoCollector collector) {
    this.collector = collector;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getAction() {
    return ACTION;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AppInfo handle(JsonObject params) {
    return collector.collect();
  }
}
