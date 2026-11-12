package com.webforj.spring.security;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.util.List;

final class HidingClassLoader extends ClassLoader {
  private final String hiddenPrefix;
  private final List<String> ownPrefixes;

  HidingClassLoader(String hiddenPrefix, String... ownPrefixes) {
    super(HidingClassLoader.class.getClassLoader());
    this.hiddenPrefix = hiddenPrefix;
    this.ownPrefixes = List.of(ownPrefixes);
  }

  @Override
  protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
    if (name.startsWith(hiddenPrefix)) {
      throw new ClassNotFoundException(name);
    }

    if (!isOwn(name)) {
      return super.loadClass(name, resolve);
    }

    synchronized (getClassLoadingLock(name)) {
      Class<?> loaded = findLoadedClass(name);
      if (loaded == null) {
        byte[] bytes = readBytes(name);
        loaded = defineClass(name, bytes, 0, bytes.length);
      }

      if (resolve) {
        resolveClass(loaded);
      }

      return loaded;
    }
  }

  private boolean isOwn(String name) {
    return ownPrefixes.stream().anyMatch(prefix -> name.startsWith(prefix) && (prefix.endsWith(".")
        || name.length() == prefix.length() || name.charAt(prefix.length()) == '$'));
  }

  private byte[] readBytes(String name) throws ClassNotFoundException {
    String resource = name.replace('.', '/') + ".class";
    try (InputStream input = getParent().getResourceAsStream(resource)) {
      if (input == null) {
        throw new ClassNotFoundException(name);
      }

      return input.readAllBytes();
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }
}
