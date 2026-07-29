package com.webforj.devtools.craftforj.styles.model;

import java.util.Map;

/**
 * Describes the resolved application stylesheet.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class StylesheetInfo {

  private String path;
  private Map<String, String> regions = Map.of();
  private boolean exists;
  private String content;
  private boolean defaultUsed;
  private String version;

  /**
   * Gets the absolute stylesheet path.
   *
   * @return the absolute path
   */
  public String getPath() {
    return path;
  }

  /**
   * Sets the absolute stylesheet path.
   *
   * @param path the absolute path
   */
  public void setPath(String path) {
    this.path = path;
  }

  /**
   * Returns whether the stylesheet exists on disk.
   *
   * @return {@code true} if the file exists
   */
  public boolean isExists() {
    return exists;
  }

  /**
   * Sets whether the stylesheet exists on disk.
   *
   * @param exists {@code true} if the file exists
   */
  public void setExists(boolean exists) {
    this.exists = exists;
  }

  /**
   * Gets the stylesheet content.
   *
   * @return the content, or {@code null} when the file does not exist
   */
  public String getContent() {
    return content;
  }

  /**
   * Sets the stylesheet content.
   *
   * @param content the content
   */
  public void setContent(String content) {
    this.content = content;
  }

  /**
   * Returns whether the path came from the layout default rather than configuration.
   *
   * @return {@code true} if the layout default was used
   */
  public boolean isDefaultUsed() {
    return defaultUsed;
  }

  /**
   * Sets whether the path came from the layout default.
   *
   * @param defaultUsed {@code true} if the layout default was used
   */
  public void setDefaultUsed(boolean defaultUsed) {
    this.defaultUsed = defaultUsed;
  }

  /**
   * Gets the content version used as the base for compare and swap writes.
   *
   * @return the version
   */
  public String getVersion() {
    return version;
  }

  /**
   * Sets the content version used as the base for compare and swap writes.
   *
   * @param version the version
   */
  public void setVersion(String version) {
    this.version = version;
  }

  /**
   * Gets the regions the stylesheet carries, by name.
   *
   * <p>
   * A region is a named stretch of the file one craftforJ writer owns.
   * </p>
   *
   * @return the region bodies by name, empty when the file carries none
   */
  public Map<String, String> getRegions() {
    return regions;
  }

  /**
   * Sets the regions the stylesheet carries.
   *
   * @param regions the region bodies by name
   */
  public void setRegions(Map<String, String> regions) {
    this.regions = regions == null ? Map.of() : regions;
  }
}
