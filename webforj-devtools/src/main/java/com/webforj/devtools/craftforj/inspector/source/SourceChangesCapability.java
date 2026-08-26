package com.webforj.devtools.craftforj.inspector.source;

import com.webforj.App;
import com.webforj.devtools.craftforj.capabilities.CraftforjCapability;
import com.webforj.devtools.craftforj.utilities.KotlinClassDetector;

/**
 * Writing property changes and route access back to the Java sources, off for a Kotlin application
 * whatever the setting says.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class SourceChangesCapability implements CraftforjCapability {

  /**
   * The key the panel receives.
   */
  public static final String KEY = "sourceCodeChanges";

  /**
   * The configuration key that switches the capability off.
   */
  public static final String CONFIG_KEY = "webforj.devtools.craftforj.source-changes";

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
    if (app != null && KotlinClassDetector.isKotlin(app.getClass())) {
      return false;
    }

    return isEnabled(CONFIG_KEY);
  }
}
