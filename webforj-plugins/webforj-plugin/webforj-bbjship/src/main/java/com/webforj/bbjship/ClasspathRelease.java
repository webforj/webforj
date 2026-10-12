package com.webforj.bbjship;

import java.io.Closeable;

/**
 * Finds what holds the files of a classpath open in BBjServices, so that an undeploy can release
 * them once nothing runs on the classpath any more.
 *
 * <p>
 * BBjServices keeps the class loader of a classpath, and with it every jar open, until the garbage
 * collector drops the loader. Windows refuses to delete an open jar. Closing the loader releases
 * the jars at once.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@FunctionalInterface
public interface ClasspathRelease {

  /**
   * Releases nothing, for a process outside BBjServices.
   */
  ClasspathRelease NONE = classpathName -> () -> {
  };

  /**
   * Finds the hold on the files of a classpath. The classpath is still registered when this is
   * called, closing the result releases the files.
   *
   * @param classpathName the classpath name
   *
   * @return the hold, closing it releases the files, a hold that does nothing when the classpath
   *         holds no files in this process
   */
  Closeable find(String classpathName);
}
