package com.webforj.devtools.craftforj.capabilities;

import com.typesafe.config.Config;

/**
 * The configuration that turns individual craftforJ features off.
 *
 * <p>
 * craftforJ decides on its own what a licensed session may do, but the features that write to the
 * project are the ones a team may not want reachable from a browser. Each of those carries a key
 * here so the application can switch it off without switching craftforJ off. Every key defaults to
 * on, so a project that says nothing keeps the full feature set.
 * </p>
 *
 * <p>
 * The keys that drive the AI assistant live under {@code webforj.devtools.craftforj.ai} so the
 * assistant stays separate from the deterministic tooling.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class FeatureFlags {

  /**
   * The configuration key that allows craftforJ to write Java sources.
   */
  public static final String KEY_SOURCE_CHANGES = "webforj.devtools.craftforj.source-changes";

  /**
   * The configuration key that allows craftforJ to write the application stylesheet.
   */
  public static final String KEY_STYLESHEET_CHANGES =
      "webforj.devtools.craftforj.stylesheet-changes";

  /**
   * The configuration key that turns the AI assistant off altogether.
   */
  public static final String KEY_AI_ENABLED = "webforj.devtools.craftforj.ai.enabled";

  /**
   * The configuration key that allows the AI assistant to write free form Java sources behind the
   * compile gate.
   */
  public static final String KEY_AI_FREEFORM_CHANGES =
      "webforj.devtools.craftforj.ai.freeform-changes";

  private final boolean sourceChanges;
  private final boolean stylesheetChanges;
  private final boolean aiEnabled;
  private final boolean aiFreeformChanges;

  private FeatureFlags(Builder builder) {
    this.sourceChanges = builder.sourceChanges;
    this.stylesheetChanges = builder.stylesheetChanges;
    this.aiEnabled = builder.aiEnabled;
    this.aiFreeformChanges = builder.aiFreeformChanges;
  }

  /**
   * Creates a builder with every feature on, which is what a project that configures nothing gets.
   *
   * @return a new builder
   */
  public static Builder builder() {
    return new Builder();
  }

  /**
   * Reads the flags from the given webforJ configuration.
   *
   * @param config the webforJ configuration, may be {@code null}
   * @return the flags filled from the configuration, on where a key is absent
   */
  public static FeatureFlags from(Config config) {
    return builder().sourceChanges(getBoolean(config, KEY_SOURCE_CHANGES, true))
        .stylesheetChanges(getBoolean(config, KEY_STYLESHEET_CHANGES, true))
        .aiEnabled(getBoolean(config, KEY_AI_ENABLED, true))
        .aiFreeformChanges(getBoolean(config, KEY_AI_FREEFORM_CHANGES, true)).build();
  }

  /**
   * Indicates whether craftforJ may write Java sources.
   *
   * @return {@code true} when Java sources may be written
   */
  public boolean isSourceChanges() {
    return sourceChanges;
  }

  /**
   * Indicates whether craftforJ may write the application stylesheet.
   *
   * @return {@code true} when the stylesheet may be written
   */
  public boolean isStylesheetChanges() {
    return stylesheetChanges;
  }

  /**
   * Indicates whether the AI assistant is available at all.
   *
   * @return {@code true} when the assistant is available
   */
  public boolean isAiEnabled() {
    return aiEnabled;
  }

  /**
   * Indicates whether the AI assistant may write free form Java sources.
   *
   * @return {@code true} when free form sources may be written
   */
  public boolean isAiFreeformChanges() {
    return aiFreeformChanges;
  }

  /**
   * Builds a set of flags, starting from everything on.
   */
  public static final class Builder {

    private boolean sourceChanges = true;
    private boolean stylesheetChanges = true;
    private boolean aiEnabled = true;
    private boolean aiFreeformChanges = true;

    private Builder() {}

    /**
     * Sets whether craftforJ may write Java sources.
     *
     * @param value whether Java sources may be written
     * @return this builder
     */
    public Builder sourceChanges(boolean value) {
      this.sourceChanges = value;
      return this;
    }

    /**
     * Sets whether craftforJ may write the application stylesheet.
     *
     * @param value whether the stylesheet may be written
     * @return this builder
     */
    public Builder stylesheetChanges(boolean value) {
      this.stylesheetChanges = value;
      return this;
    }

    /**
     * Sets whether the AI assistant is available at all.
     *
     * @param value whether the assistant is available
     * @return this builder
     */
    public Builder aiEnabled(boolean value) {
      this.aiEnabled = value;
      return this;
    }

    /**
     * Sets whether the AI assistant may write free form Java sources.
     *
     * @param value whether free form sources may be written
     * @return this builder
     */
    public Builder aiFreeformChanges(boolean value) {
      this.aiFreeformChanges = value;
      return this;
    }

    /**
     * Builds the flags.
     *
     * @return the flags
     */
    public FeatureFlags build() {
      return new FeatureFlags(this);
    }
  }

  private static boolean getBoolean(Config config, String key, boolean defaultValue) {
    if (config != null && config.hasPath(key) && !config.getIsNull(key)) {
      return config.getBoolean(key);
    }

    return defaultValue;
  }
}
