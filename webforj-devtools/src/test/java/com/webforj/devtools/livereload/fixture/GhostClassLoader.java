package com.webforj.devtools.livereload.fixture;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.net.URL;

/**
 * A class loader that defines one class itself and reports no class file for it.
 */
public final class GhostClassLoader extends ClassLoader {

  private final String ghostName;
  private Class<?> ghost;

  public GhostClassLoader(String ghostName) {
    super(GhostClassLoader.class.getClassLoader());
    this.ghostName = ghostName;
  }

  @Override
  public URL getResource(String name) {
    return name.equals(toResourceName(ghostName)) ? null : super.getResource(name);
  }

  @Override
  protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
    if (!ghostName.equals(name)) {
      return super.loadClass(name, resolve);
    }

    synchronized (getClassLoadingLock(name)) {
      if (ghost == null) {
        byte[] bytes = readBytes(name);
        ghost = defineClass(name, bytes, 0, bytes.length);
      }

      return ghost;
    }
  }

  private byte[] readBytes(String name) {
    try (InputStream input = getParent().getResourceAsStream(toResourceName(name))) {
      return input.readAllBytes();
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  private static String toResourceName(String className) {
    return className.replace('.', '/') + ".class";
  }
}
