package com.webforj.devtools.craftforj.styles;

import com.typesafe.config.Config;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.devtools.craftforj.capabilities.CraftforjCapability;

/**
 * Saving themes and styles into the application stylesheet.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class StylesheetChangesCapability implements CraftforjCapability {

  /**
   * The key the panel receives.
   */
  public static final String KEY = "stylesheetChanges";

  /**
   * The configuration key that switches the capability off.
   */
  public static final String CONFIG_KEY = "webforj.devtools.craftforj.stylesheet-changes";

  /**
   * {@inheritDoc}
   */
  @Override
  public String getKey() {
    return KEY;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isSupported(App app) {
    Environment environment = Environment.getCurrent();
    Config config = environment == null ? null : environment.getConfig();

    return config == null || !config.hasPath(CONFIG_KEY) || config.getBoolean(CONFIG_KEY);
  }
}
