package com.webforj.bbjship;

import com.basis.startup.BBjClassLoader;
import com.basis.startup.ExternalClassLoader;
import com.basis.startup.type.BBjException;
import java.io.Closeable;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;

/**
 * Releases the files of a classpath inside BBjServices by closing the class loader BBjServices
 * built for it.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjServicesClasspathRelease implements ClasspathRelease {

  private static final Logger LOGGER =
      System.getLogger(BBjServicesClasspathRelease.class.getName());

  /**
   * {@inheritDoc}
   */
  @Override
  public Closeable find(String classpathName) {
    try {
      ExternalClassLoader loader = BBjClassLoader.instance().getExternalClassLoader(classpathName);
      if (loader.getClassLoader() instanceof Closeable hold) {
        return hold;
      }
    } catch (BBjException | RuntimeException unavailable) {
      LOGGER.log(Level.WARNING, "The class loader of the BBj classpath " + classpathName
          + " could not be found, its files stay open until BBjServices restarts", unavailable);
    }

    return () -> {
    };
  }
}
