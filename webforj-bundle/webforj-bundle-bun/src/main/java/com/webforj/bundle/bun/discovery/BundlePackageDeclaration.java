package com.webforj.bundle.bun.discovery;

/**
 * One npm package required by the project as captured from the classpath scan.
 *
 * <p>
 * Carries the npm name, the semver range that will land in the generated package.json, and whether
 * the package is a build time dependency.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class BundlePackageDeclaration {

  private String name = null;
  private String version = null;
  private boolean dev = false;

  /**
   * Sets the npm package name.
   *
   * @param name the npm package name
   * @return the declaration
   */
  public BundlePackageDeclaration setName(String name) {
    this.name = name;

    return this;
  }

  /**
   * Gets the npm package name.
   *
   * @return the npm package name
   * @see #setName(String)
   */
  public String getName() {
    return name;
  }

  /**
   * Sets the npm semver range.
   *
   * @param version the npm semver range
   * @return the declaration
   */
  public BundlePackageDeclaration setVersion(String version) {
    this.version = version;

    return this;
  }

  /**
   * Gets the npm semver range.
   *
   * @return the npm semver range
   * @see #setVersion(String)
   */
  public String getVersion() {
    return version;
  }

  /**
   * Sets whether the package is a build time dependency rather than a runtime browser dependency.
   *
   * @param dev {@code true} for a build time dependency
   * @return the declaration
   */
  public BundlePackageDeclaration setDev(boolean dev) {
    this.dev = dev;

    return this;
  }

  /**
   * Indicates whether the package is a build time dependency rather than a runtime browser
   * dependency.
   *
   * @return {@code true} for a build time dependency
   * @see #setDev(boolean)
   */
  public boolean isDev() {
    return dev;
  }
}
