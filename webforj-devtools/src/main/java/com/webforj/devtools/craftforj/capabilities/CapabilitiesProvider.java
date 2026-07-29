package com.webforj.devtools.craftforj.capabilities;

import com.typesafe.config.Config;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.tools.ToolProvider;

/**
 * Determines server capabilities based on the license status and the feature flags read from the
 * application configuration.
 *
 * <p>
 * Capabilities are computed once at startup and used to gate both action registration and the
 * capabilities response sent to the frontend. When unlicensed, capabilities are empty and no
 * features are available. When licensed, a capability is announced unless the application switched
 * it off through {@link FeatureFlags}.
 * </p>
 *
 * <p>
 * craftforJ is released from the webforJ reactor, so every feature shipped today is present in the
 * framework it ships with and needs no version check. {@link #isFrameworkAtLeast(int, int)} reads
 * the framework version on the classpath and is there for a future feature that needs a release
 * newer than the one that introduced it.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class CapabilitiesProvider {

  /**
   * Capability key for source code changes (switched off through
   * {@link FeatureFlags#KEY_SOURCE_CHANGES}).
   */
  public static final String CAPABILITY_SOURCE_CODE_CHANGES = "sourceCodeChanges";

  /**
   * Capability key for application stylesheet changes (switched off through
   * {@link FeatureFlags#KEY_STYLESHEET_CHANGES}).
   */
  public static final String CAPABILITY_STYLESHEET_CHANGES = "stylesheetChanges";

  /**
   * Capability key for the AI assistant (switched off through {@link FeatureFlags#KEY_AI_ENABLED}).
   */
  public static final String CAPABILITY_AI_ASSISTANT = "aiAssistant";

  /**
   * Capability key for free form source changes behind the compile gate (switched off through
   * {@link FeatureFlags#KEY_AI_FREEFORM_CHANGES}).
   */
  public static final String CAPABILITY_SOURCE_FREEFORM_CHANGES = "sourceFreeformChanges";

  /**
   * Compile gate level when a system compiler validates staged sources.
   */
  public static final String COMPILE_GATE_FULL = "full";

  /**
   * Compile gate level when only parse validation is available.
   */
  public static final String COMPILE_GATE_PARSE_ONLY = "parse-only";

  private final VersionDetector versionDetector;
  private final FrameworkVersionDetector frameworkVersion;
  private final FeatureFlags features;
  private final boolean licensed;
  private final List<String> capabilities;

  /**
   * Creates a new provider using the default version detectors.
   *
   * @param config the webforJ configuration used to read the feature flags, may be {@code null}
   * @param licensed whether a valid license is present
   */
  public CapabilitiesProvider(Config config, boolean licensed) {
    this(new VersionDetector(), new FrameworkVersionDetector(), FeatureFlags.from(config),
        licensed);
  }

  /**
   * Creates a new provider with the given detectors and feature flags.
   *
   * @param versionDetector the craftforJ module version detector, used for reporting
   * @param frameworkVersion the framework version detector, kept for future version gated features
   * @param features the feature flags read from the application configuration
   * @param licensed whether a valid license is present
   */
  CapabilitiesProvider(VersionDetector versionDetector, FrameworkVersionDetector frameworkVersion,
      FeatureFlags features, boolean licensed) {
    this.versionDetector = versionDetector;
    this.frameworkVersion = frameworkVersion;
    this.features = features;
    this.licensed = licensed;
    this.capabilities = licensed ? computeCapabilities() : Collections.emptyList();
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
   * Gets the list of supported capabilities.
   *
   * @return an unmodifiable list of capability keys
   */
  public List<String> getCapabilities() {
    return capabilities;
  }

  /**
   * Checks whether a capability is supported.
   *
   * @param capability the capability key
   * @return {@code true} if the capability is supported
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
   * Computes the capabilities the feature flags leave available.
   *
   * @return the list of supported capability keys
   */
  private List<String> computeCapabilities() {
    List<String> result = new ArrayList<>();

    if (features.isSourceChanges()) {
      result.add(CAPABILITY_SOURCE_CODE_CHANGES);

      // Free form editing writes Java sources through the assistant, so it needs both flags
      if (features.isAiEnabled() && features.isAiFreeformChanges()) {
        result.add(CAPABILITY_SOURCE_FREEFORM_CHANGES);
      }
    }

    if (features.isStylesheetChanges()) {
      result.add(CAPABILITY_STYLESHEET_CHANGES);
    }

    if (features.isAiEnabled()) {
      result.add(CAPABILITY_AI_ASSISTANT);
    }

    return Collections.unmodifiableList(result);
  }
}
