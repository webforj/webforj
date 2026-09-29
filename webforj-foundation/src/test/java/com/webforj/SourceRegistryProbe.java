package com.webforj;

import com.webforj.component.ComponentSourceRegistry;

/**
 * Registers objects from outside the packages the source registry leaves out of a chain.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class SourceRegistryProbe {

  private SourceRegistryProbe() {}

  /**
   * Creates an object and registers it.
   *
   * @return the registered object
   */
  public static Object create() {
    Object created = new Object();
    ComponentSourceRegistry.register(created);

    return created;
  }

  /**
   * Creates an object through another method of this class.
   *
   * @return the registered object
   */
  public static Object createThrough() {
    return create();
  }
}
