package sun.webforj;

import com.webforj.component.ComponentSourceRegistry;

/**
 * Registers objects from a package the source registry leaves out of a chain.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class SunRegistryProbe {

  private SunRegistryProbe() {}

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
}
