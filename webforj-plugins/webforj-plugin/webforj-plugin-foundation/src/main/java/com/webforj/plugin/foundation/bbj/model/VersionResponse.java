package com.webforj.plugin.foundation.bbj.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Holds the version answer of BBjShip.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class VersionResponse {

  private String version;
  private List<String> javaArguments;

  /**
   * Gets the BBjShip version.
   *
   * @return the BBjShip version, null when the answer carries none
   */
  public String getVersion() {
    return version;
  }

  /**
   * Gets the Java arguments BBjServices runs with.
   *
   * @return the unmodifiable arguments, empty when the answer carries none
   */
  public List<String> getJavaArguments() {
    return javaArguments == null ? List.of()
        : Collections.unmodifiableList(new ArrayList<>(javaArguments));
  }
}
