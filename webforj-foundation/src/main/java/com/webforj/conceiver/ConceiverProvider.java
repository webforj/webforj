package com.webforj.conceiver;

import com.webforj.environment.ObjectTable;
import java.util.ServiceLoader;

/**
 * Provides access to the current {@link Conceiver}
 *
 * @since 24.12
 * @author Hyyan Abo Fakher
 */
public final class ConceiverProvider {
  public static final String LOOKUP_KEY = "com.webforj.conceiver.Conceiver.instance";

  private ConceiverProvider() {}

  /**
   * Get the current {@link Conceiver}.
   *
   * @return the current {@link Conceiver}.
   */
  public static Conceiver getCurrent() {
    if (!ObjectTable.contains(LOOKUP_KEY)) {
      ServiceLoader<Conceiver> loader = ServiceLoader.load(Conceiver.class);
      ObjectTable.put(LOOKUP_KEY, loader.findFirst().orElse(new DefaultConceiver()));
    }

    return (Conceiver) ObjectTable.get(LOOKUP_KEY);
  }
}
