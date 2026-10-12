package com.webforj.bbjship.model;

import com.webforj.bbjship.ApplicationConfiguration;
import java.nio.file.Path;
import java.util.List;

/**
 * Holds the files and configuration of a prepared BBj deployment.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class InstalledDeployment {

  private final Path directory;
  private final List<Path> classpath;
  private final ApplicationConfiguration configuration;
  private final List<Path> classFolders;

  private InstalledDeployment(Builder builder) {
    this.directory = builder.directory;
    this.classpath = List.copyOf(builder.classpath);
    this.configuration = builder.configuration;
    this.classFolders = List.copyOf(builder.classFolders);
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
   * Gets the application classpath.
   *
   * @return the application classpath
   */
  public List<Path> getClasspath() {
    return classpath;
  }

  /**
   * Gets the configuration the application carries.
   *
   * @return the application configuration
   */
  public ApplicationConfiguration getConfiguration() {
    return configuration;
  }

  /**
   * Gets the class folders a development run loads the application from.
   *
   * @return the class folders in classpath order, empty for an archive deployment
   */
  public List<Path> getClassFolders() {
    return classFolders;
  }

  /**
   * Builds a prepared deployment.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  public static final class Builder {

    private Path directory;
    private List<Path> classpath = List.of();
    private ApplicationConfiguration configuration;
    private List<Path> classFolders = List.of();

    private Builder() {}

    /**
     * Creates a builder.
     *
     * @return the builder
     */
    public static Builder create() {
      return new Builder();
    }

    /**
     * Sets the deployment directory.
     *
     * @param directory the deployment directory
     *
     * @return this builder
     */
    public Builder setDirectory(Path directory) {
      this.directory = directory;
      return this;
    }

    /**
     * Sets the application classpath.
     *
     * @param classpath the application classpath
     *
     * @return this builder
     */
    public Builder setClasspath(List<Path> classpath) {
      this.classpath = classpath;
      return this;
    }

    /**
     * Sets the application configuration.
     *
     * @param configuration the application configuration
     *
     * @return this builder
     */
    public Builder setConfiguration(ApplicationConfiguration configuration) {
      this.configuration = configuration;
      return this;
    }

    /**
     * Sets the class folders a development run loads the application from.
     *
     * @param classFolders the class folders in classpath order
     *
     * @return this builder
     * @throws NullPointerException if classFolders or one of its folders is null
     */
    public Builder setClassFolders(List<Path> classFolders) {
      this.classFolders = List.copyOf(classFolders);
      return this;
    }

    /**
     * Builds the prepared deployment.
     *
     * @return the prepared deployment
     */
    public InstalledDeployment build() {
      return new InstalledDeployment(this);
    }
  }
}
