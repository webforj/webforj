package com.webforj.bbjship.assets.model;

import java.util.Objects;

/**
 * Describes one asset request of a published application.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class AssetRequest {

  private final String applicationName;
  private final String path;
  private final String padding;
  private final String background;
  private final String ifNoneMatch;

  private AssetRequest(Builder builder) {
    this.applicationName = builder.applicationName;
    this.path = builder.path;
    this.padding = builder.padding;
    this.background = builder.background;
    this.ifNoneMatch = builder.ifNoneMatch;
  }

  /**
   * Starts a request for a path of a published application.
   *
   * @param applicationName the published application name
   * @param path the requested path below the application, without a leading slash
   *
   * @return the builder
   * @throws NullPointerException if the application name or the path is null
   */
  public static Builder create(String applicationName, String path) {
    return new Builder(applicationName, path);
  }

  /**
   * Returns the published application name.
   *
   * @return the application name
   */
  public String getApplicationName() {
    return applicationName;
  }

  /**
   * Returns the requested path below the application.
   *
   * @return the path
   */
  public String getPath() {
    return path;
  }

  /**
   * Returns the requested icon padding or null.
   *
   * @return the padding parameter
   */
  public String getPadding() {
    return padding;
  }

  /**
   * Returns the requested icon background or null.
   *
   * @return the background parameter
   */
  public String getBackground() {
    return background;
  }

  /**
   * Returns the entity tag the client already holds or null.
   *
   * @return the If-None-Match header value
   */
  public String getIfNoneMatch() {
    return ifNoneMatch;
  }

  /**
   * Builds an asset request.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  public static final class Builder {

    private final String applicationName;
    private final String path;
    private String padding;
    private String background;
    private String ifNoneMatch;

    private Builder(String applicationName, String path) {
      this.applicationName = Objects.requireNonNull(applicationName);
      this.path = Objects.requireNonNull(path);
    }

    /**
     * Sets the icon padding parameter.
     *
     * @param padding the padding parameter or null
     *
     * @return this builder
     */
    public Builder setPadding(String padding) {
      this.padding = padding;
      return this;
    }

    /**
     * Sets the icon background parameter.
     *
     * @param background the background parameter or null
     *
     * @return this builder
     */
    public Builder setBackground(String background) {
      this.background = background;
      return this;
    }

    /**
     * Sets the entity tag the client already holds.
     *
     * @param ifNoneMatch the If-None-Match header value or null
     *
     * @return this builder
     */
    public Builder setIfNoneMatch(String ifNoneMatch) {
      this.ifNoneMatch = ifNoneMatch;
      return this;
    }

    /**
     * Builds the request.
     *
     * @return the request
     */
    public AssetRequest build() {
      return new AssetRequest(this);
    }
  }
}
