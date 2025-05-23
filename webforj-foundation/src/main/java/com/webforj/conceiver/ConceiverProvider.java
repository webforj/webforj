package com.webforj.conceiver;

import com.webforj.environment.ObjectTable;
import java.util.ServiceLoader;

/**
 * Provides access to the current {@link Conceiver}.
 *
 * <p>
 * The current {@link Conceiver} is the first instance of the {@link Conceiver} found in the
 * {@code META-INF/services/com.webforj.conceiver.Conceiver} file.
 * </p>
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
    ClassLoader tccl = Thread.currentThread().getContextClassLoader();

    if (tccl != null) {
      ServiceLoader<Conceiver> tcclLoader = ServiceLoader.load(Conceiver.class, tccl);
      Conceiver tcclConceiver = tcclLoader.findFirst().orElse(null);
      if (tcclConceiver != null) {
        return tcclConceiver;
      }
    }

    // Fallback to ObjectTable caching mechanism
    if (!ObjectTable.contains(LOOKUP_KEY)) {
      ServiceLoader<Conceiver> appClassLoaderLoader =
          ServiceLoader.load(Conceiver.class, ConceiverProvider.class.getClassLoader());
      ObjectTable.put(LOOKUP_KEY,
          appClassLoaderLoader.findFirst().orElse(new DefaultConceiver()));
    }

    return (Conceiver) ObjectTable.get(LOOKUP_KEY);
  }
}
