package com.webforj.bbjship.model;

import java.nio.file.Path;
import java.util.List;

/**
 * Describes an application published by BBjShip.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@SuppressWarnings("java:S6206")
public final class PublishedApplication {

  private final String name;
  private final String url;
  private final Path directory;
  private final List<Path> classFolders;

  /**
   * Creates a published application.
   *
   * @param name the published application name
   * @param url the context path of the published application
   * @param directory the deployment directory
   * @param classFolders the class folders of a development run in classpath order, null or empty
   *        for an archive deployment
   */
  public PublishedApplication(String name, String url, Path directory, List<Path> classFolders) {
    this.name = name;
    this.url = url;
    this.directory = directory;
    this.classFolders = classFolders == null ? List.of() : List.copyOf(classFolders);
  }

  /**
   * Gets the published application name.
   *
   * @return the published application name
   */
  public String getName() {
    return name;
  }

  /**
   * Gets the context path of the published application.
   *
   * @return the context path
   */
  public String getUrl() {
    return url;
  }

  /**
   * Gets the deployment directory.
   *
   * @return the deployment directory
   */
  public Path getDirectory() {
    return directory;
  }

  /**
   * Gets the class folders a development run loads the application from.
   *
   * @return the class folders in classpath order, empty for an archive deployment
   */
  public List<Path> getClassFolders() {
    return classFolders;
  }
}
