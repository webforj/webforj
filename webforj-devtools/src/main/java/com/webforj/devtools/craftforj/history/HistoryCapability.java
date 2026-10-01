package com.webforj.devtools.craftforj.history;

import com.webforj.App;
import com.webforj.devtools.craftforj.capabilities.CraftforjCapability;
import com.webforj.devtools.craftforj.inspector.source.SourceFreeformChangesCapability;
import com.webforj.devtools.craftforj.source.SourceChangesCapability;
import com.webforj.devtools.craftforj.styles.StylesheetChangesCapability;
import java.util.List;

/**
 * Undo and redo of the changes craftforJ writes to the project.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class HistoryCapability implements CraftforjCapability {

  /**
   * The key the panel receives.
   */
  public static final String KEY = "history";

  private final List<CraftforjCapability> writes;

  /**
   * Creates the capability, supported while any capability that writes to the project is.
   */
  public HistoryCapability() {
    this(List.of(new SourceChangesCapability(), new SourceFreeformChangesCapability(),
        new StylesheetChangesCapability()));
  }

  HistoryCapability(List<CraftforjCapability> writes) {
    this.writes = List.copyOf(writes);
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
    return writes.stream().anyMatch(write -> write.isSupported(app));
  }
}
