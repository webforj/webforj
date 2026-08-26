package com.webforj.devtools.craftforj.inspector.source;

import com.webforj.App;
import com.webforj.devtools.craftforj.ai.AiAssistantCapability;
import com.webforj.devtools.craftforj.capabilities.CraftforjCapability;

/**
 * The assistant writing Java of its own behind the compile gate, which needs both source changes
 * and the assistant.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class SourceFreeformChangesCapability implements CraftforjCapability {

  /**
   * The key the panel receives.
   */
  public static final String KEY = "sourceFreeformChanges";

  /**
   * The configuration key that switches the capability off.
   */
  public static final String CONFIG_KEY = "webforj.devtools.craftforj.ai.freeform-changes";

  private final CraftforjCapability sourceChanges;
  private final CraftforjCapability aiAssistant;

  /**
   * Creates the capability over the source changes and assistant checks.
   */
  public SourceFreeformChangesCapability() {
    this(new SourceChangesCapability(), new AiAssistantCapability());
  }

  /**
   * Creates the capability over the given checks.
   *
   * @param sourceChanges the source changes check
   * @param aiAssistant the assistant check
   */
  SourceFreeformChangesCapability(CraftforjCapability sourceChanges,
      CraftforjCapability aiAssistant) {
    this.sourceChanges = sourceChanges;
    this.aiAssistant = aiAssistant;
  }

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
    if (!sourceChanges.isSupported(app) || !aiAssistant.isSupported(app)) {
      return false;
    }

    return isEnabled(CONFIG_KEY);
  }
}
