package com.webforj.devtools.craftforj.capabilities;

import com.webforj.App;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.ServiceLoader;
import java.util.Set;
import javax.tools.ToolProvider;

/**
 * Announces the capabilities the panel may use, decided once at startup by asking every
 * {@link CraftforjCapability} a module declares under
 * {@code META-INF/services/com.webforj.devtools.craftforj.capabilities.CraftforjCapability}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class CapabilitiesProvider {

  /**
   * Compile gate level when a system compiler validates staged sources.
   */
  public static final String COMPILE_GATE_FULL = "full";

  /**
   * Compile gate level when only parse validation is available.
   */
  public static final String COMPILE_GATE_PARSE_ONLY = "parse-only";

  /**
   * The system property the build plugin sets to name the attached hotswap tool.
   */
  public static final String HOTSWAP_TOOL_PROPERTY = "webforj.hotswap.tool";

  /**
   * The system property the build plugin sets to name the depth of the class updates.
   */
  public static final String HOTSWAP_LEVEL_PROPERTY = "webforj.hotswap.level";

  private final VersionDetector versionDetector;
  private final FrameworkVersionDetector frameworkVersion;
  private final boolean licensed;
  private final List<String> capabilities;

  /**
   * Creates a provider asking the declared capabilities about the given application.
   *
   * @param app the running application
   * @param licensed whether a valid license is present
   */
  public CapabilitiesProvider(App app, boolean licensed) {
    this(create(app, licensed));
  }

  private CapabilitiesProvider(Builder builder) {
    this.versionDetector = builder.versionDetector;
    this.frameworkVersion = builder.frameworkVersion;
    this.licensed = builder.licensed;
    this.capabilities =
        licensed ? decide(builder.capabilities, builder.app) : Collections.emptyList();
  }

  /**
   * Starts a provider over the declared capabilities and the default version detectors.
   *
   * @param app the running application
   * @param licensed whether a valid license is present
   * @return the builder
   */
  static Builder create(App app, boolean licensed) {
    return new Builder(app, licensed);
  }

  /**
   * Gets the craftforJ version string.
   *
   * @return the version string, or {@code null} if unknown
   */
  public String getVersion() {
    return versionDetector.getVersion();
  }

  /**
   * Checks whether a valid license is present.
   *
   * @return {@code true} if licensed
   */
  public boolean isLicensed() {
    return licensed;
  }

  /**
   * Gets the announced capabilities, in declaration order.
   *
   * @return an unmodifiable list of capability keys
   */
  public List<String> getCapabilities() {
    return capabilities;
  }

  /**
   * Checks whether a capability is announced.
   *
   * @param capability the capability key
   * @return {@code true} if the capability is announced
   */
  public boolean isSupported(String capability) {
    return capabilities.contains(capability);
  }

  /**
   * Checks whether the framework version on the classpath is at least the given major and minor.
   *
   * @param major the minimum major version
   * @param minor the minimum minor version
   * @return {@code true} if the framework meets the requirement
   */
  public boolean isFrameworkAtLeast(int major, int minor) {
    return frameworkVersion.isAtLeast(major, minor);
  }

  /**
   * Gets the compile gate level available on this runtime.
   *
   * @return {@link #COMPILE_GATE_FULL} when a system compiler is present, otherwise
   *         {@link #COMPILE_GATE_PARSE_ONLY}
   */
  public String getCompileGate() {
    return ToolProvider.getSystemJavaCompiler() != null ? COMPILE_GATE_FULL
        : COMPILE_GATE_PARSE_ONLY;
  }

  /**
   * Gets the hotswap tool the build plugin attached to this run.
   *
   * @return {@code hotswapAgent} or {@code jrebel}, or {@code null} when no tool is attached
   *
   * @since 26.02
   */
  public String getHotswapTool() {
    return System.getProperty(HOTSWAP_TOOL_PROPERTY);
  }

  /**
   * Gets the depth of the class updates the attached tool applies on this virtual machine.
   *
   * @return {@code full} or {@code limited}, or {@code null} when no tool is attached
   *
   * @since 26.02
   */
  public String getHotswapLevel() {
    return System.getProperty(HOTSWAP_LEVEL_PROPERTY);
  }

  /**
   * Loads the capabilities the modules declare as services.
   *
   * @return the capabilities, in declaration order
   */
  static List<CraftforjCapability> loadCapabilities() {
    return ServiceLoader.load(CraftforjCapability.class).stream().map(ServiceLoader.Provider::get)
        .toList();
  }

  /**
   * Asks every capability about the application and collects the announced keys.
   *
   * @param declared the capabilities, in declaration order
   * @param app the running application
   * @return the announced keys, in declaration order
   *
   * @throws IllegalStateException when a key is declared twice
   */
  static List<String> decide(List<CraftforjCapability> declared, App app) {
    Set<String> keys = new LinkedHashSet<>();
    List<String> announced = new ArrayList<>();
    for (CraftforjCapability capability : declared) {
      if (!keys.add(capability.getKey())) {
        throw new IllegalStateException(
            "The capability " + capability.getKey() + " is declared twice");
      }

      if (capability.isSupported(app)) {
        announced.add(capability.getKey());
      }
    }

    return Collections.unmodifiableList(announced);
  }

  /**
   * Builds a {@link CapabilitiesProvider}, starting from the declared capabilities and the default
   * version detectors.
   */
  static final class Builder {

    private final App app;
    private final boolean licensed;
    private VersionDetector versionDetector = new VersionDetector();
    private FrameworkVersionDetector frameworkVersion = new FrameworkVersionDetector();
    private List<CraftforjCapability> capabilities = loadCapabilities();

    private Builder(App app, boolean licensed) {
      this.app = app;
      this.licensed = licensed;
    }

    /**
     * Sets the craftforJ version detector.
     *
     * @param versionDetector the detector
     * @return this builder
     */
    Builder setVersionDetector(VersionDetector versionDetector) {
      this.versionDetector = versionDetector;
      return this;
    }

    /**
     * Sets the framework version detector.
     *
     * @param frameworkVersion the detector
     * @return this builder
     */
    Builder setFrameworkVersionDetector(FrameworkVersionDetector frameworkVersion) {
      this.frameworkVersion = frameworkVersion;
      return this;
    }

    /**
     * Sets the capabilities to ask instead of the discovered ones.
     *
     * @param capabilities the capabilities, in declaration order
     * @return this builder
     */
    Builder setCapabilities(List<CraftforjCapability> capabilities) {
      this.capabilities = List.copyOf(capabilities);
      return this;
    }

    /**
     * Builds the provider.
     *
     * @return the provider
     */
    CapabilitiesProvider build() {
      return new CapabilitiesProvider(this);
    }
  }
}
