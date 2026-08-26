package com.webforj.devtools.craftforj.ai;

import com.typesafe.config.Config;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.devtools.craftforj.capabilities.CraftforjCapability;

/**
 * The AI assistant.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class AiAssistantCapability implements CraftforjCapability {

  /**
   * The key the panel receives.
   */
  public static final String KEY = "aiAssistant";

  /**
   * The configuration key that switches the capability off.
   */
  public static final String CONFIG_KEY = "webforj.devtools.craftforj.ai.enabled";

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
