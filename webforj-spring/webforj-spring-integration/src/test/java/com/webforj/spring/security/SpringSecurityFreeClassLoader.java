package com.webforj.spring.security;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;

final class SpringSecurityFreeClassLoader extends ClassLoader {
  private static final String HIDDEN_PACKAGE = "org.springframework.security.";
  private static final String OWN_PACKAGE = "com.webforj.spring.security.";

  SpringSecurityFreeClassLoader() {
    super(SpringSecurityFreeClassLoader.class.getClassLoader());
  }

  @Override
  protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
    if (name.startsWith(HIDDEN_PACKAGE)) {
      throw new ClassNotFoundException(name);
    }

    if (!name.startsWith(OWN_PACKAGE)) {
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
