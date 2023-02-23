package org.dwcj.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.stream.Collectors;

import org.dwcj.Environment;

/**
 * A helper class to deal with assets in the classpath
 * 
 * @author Hyyan Abo Fakher
 */
public class Assets {

  private Assets() {
  }

  /**
   * Get the content of a resource as a String
   * 
   * @param path The path to the resource
   * @return The content of the resource as a String
   */
  public static String contentOf(String path) {
    ClassLoader classLoader = Environment.getInstance().getClass().getClassLoader();
    try (InputStream is = classLoader.getResourceAsStream(path)) {
      if (is == null)
        throw new IllegalArgumentException("Resource not found: " + path);

      try (InputStreamReader isr = new InputStreamReader(is);
          BufferedReader reader = new BufferedReader(isr)) {
        return reader.lines().collect(Collectors.joining(System.lineSeparator()));
      }
    } catch (IOException e) {
      throw new RuntimeException(e); // NOSONAR
    }
  }
}
