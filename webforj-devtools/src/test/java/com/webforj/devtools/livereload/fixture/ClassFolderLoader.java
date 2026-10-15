package com.webforj.devtools.livereload.fixture;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Loads one class from a class folder of its own and counts the lookups of its class file.
 */
public final class ClassFolderLoader extends ClassLoader {

  private final String className;
  private final Path folder;
  private int lookups;
  private Class<?> loaded;

  /**
   * Creates the loader of the given class, whose class file lives in the given class folder.
   *
   * @param className the binary name of the class
   * @param folder the class folder
   */
  public ClassFolderLoader(String className, Path folder) {
    super(ClassFolderLoader.class.getClassLoader());
    this.className = className;
    this.folder = folder;
  }

  /**
   * Returns how often the class file of the class was looked up.
   *
   * @return the number of lookups
   */
  public int getLookups() {
    return lookups;
  }

  /**
   * Finds the class file of the class in the class folder, other resources through the parent.
   */
  @Override
  public URL getResource(String name) {
    if (!name.equals(toResourceName(className))) {
      return super.getResource(name);
    }

    lookups++;
    Path file = folder.resolve(name);
    try {
      return Files.exists(file) ? file.toUri().toURL() : null;
    } catch (MalformedURLException e) {
      throw new UncheckedIOException(e);
    }
  }

  @Override
  protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
    if (!className.equals(name)) {
      return super.loadClass(name, resolve);
    }

    synchronized (getClassLoadingLock(name)) {
      if (loaded == null) {
        byte[] bytes = readBytes(name);
        loaded = defineClass(name, bytes, 0, bytes.length);
      }

      return loaded;
    }
  }

  private byte[] readBytes(String name) {
    try {
      return Files.readAllBytes(folder.resolve(toResourceName(name)));
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  private static String toResourceName(String className) {
    return className.replace('.', '/') + ".class";
  }
}
