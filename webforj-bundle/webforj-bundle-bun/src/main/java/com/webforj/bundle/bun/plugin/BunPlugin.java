package com.webforj.bundle.bun.plugin;

/**
 * A Bun build plugin an extension contributes, carried to the build driver.
 *
 * <p>
 * The {@code wrapper} module default-exports a factory that returns a ready {@code Bun.build}
 * plugin. The build writes the wrapper next to the driver and loads it by id, passing the per id
 * options the project declared in its {@code bun.config.ts}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class BunPlugin {
  private String id = null;
  private String wrapper = null;
  private byte[] wrapperContent = null;

  /**
   * Sets the plugin id, also the key the project passes options by.
   *
   * @param id the plugin id
   * @return the plugin
   */
  public BunPlugin setId(String id) {
    this.id = id;

    return this;
  }

  /**
   * Gets the plugin id.
   *
   * @return the plugin id
   * @see #setId(String)
   */
  public String getId() {
    return id;
  }

  /**
   * Sets the path the wrapper module is written and loaded by, relative to the driver plugins
   * directory.
   *
   * @param wrapper the wrapper module path
   * @return the plugin
   */
  public BunPlugin setWrapper(String wrapper) {
    this.wrapper = wrapper;

    return this;
  }

  /**
   * Gets the path the wrapper module is written and loaded by, relative to the driver plugins
   * directory.
   *
   * @return the wrapper module path
   * @see #setWrapper(String)
   */
  public String getWrapper() {
    return wrapper;
  }

  /**
   * Sets the wrapper module source the build writes.
   *
   * @param wrapperContent the wrapper module source bytes
   * @return the plugin
   */
  public BunPlugin setWrapperContent(byte[] wrapperContent) {
    this.wrapperContent = wrapperContent;

    return this;
  }

  /**
   * Gets the wrapper module source the build writes.
   *
   * @return the wrapper module source bytes
   * @see #setWrapperContent(byte[])
   */
  public byte[] getWrapperContent() {
    return wrapperContent;
  }
}
