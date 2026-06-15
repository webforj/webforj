package com.webforj.bundle.bun.runtime;

import java.nio.file.Path;

/**
 * Builds a {@link BunRuntime} from its inputs.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class BunRuntimeBuilder {

  private Path cacheRoot = null;
  private String version = null;
  private Path overridePath = null;
  private String releaseHost = BunRuntime.DEFAULT_RELEASE_HOST;

  /**
   * Sets the per user cache root, typically {@code ~/.webforj/bun}.
   *
   * @param cacheRoot the per user cache root
   * @return the builder
   */
  public BunRuntimeBuilder setCacheRoot(Path cacheRoot) {
    this.cacheRoot = cacheRoot;

    return this;
  }

  /**
   * Sets the requested Bun version without the leading {@code v}.
   *
   * @param version the requested Bun version
   * @return the builder
   */
  public BunRuntimeBuilder setVersion(String version) {
    this.version = version;

    return this;
  }

  /**
   * Sets an explicit binary path that bypasses download, may be null.
   *
   * @param overridePath the explicit binary path
   * @return the builder
   */
  public BunRuntimeBuilder setOverridePath(Path overridePath) {
    this.overridePath = overridePath;

    return this;
  }

  /**
   * Sets the host the release archive is downloaded from.
   *
   * @param releaseHost the release host
   * @return the builder
   */
  public BunRuntimeBuilder setReleaseHost(String releaseHost) {
    this.releaseHost = releaseHost;

    return this;
  }

  /**
   * Builds the Bun runtime.
   *
   * @return a new Bun runtime
   */
  public BunRuntime build() {
    return new BunRuntime(cacheRoot, version, overridePath, releaseHost);
  }
}
