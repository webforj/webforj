package com.webforj.devtools.craftforj.capabilities.action;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import java.util.List;

/**
 * Action handler that returns server capabilities.
 *
 * <p>
 * Reports the craftforJ version and which optional features are available. The frontend uses this
 * to gate UI elements: features not in the capabilities list are disabled.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class GetCapabilitiesAction
    implements CraftforjActionHandler<GetCapabilitiesAction.Response> {

  /**
   * The action name for this handler.
   */
  public static final String ACTION = "capabilities.getCapabilities";

  private final String version;
  private final boolean licensed;
  private final List<String> capabilities;
  private final String compileGate;

  /**
   * Creates a new GetCapabilitiesAction.
   *
   * @param version the craftforJ version string, or {@code null} if unknown
   * @param licensed whether a valid license is present
   * @param capabilities the list of supported capability keys
   */
  public GetCapabilitiesAction(String version, boolean licensed, List<String> capabilities) {
    this(version, licensed, capabilities, null);
  }

  /**
   * Creates a new GetCapabilitiesAction with a compile gate level.
   *
   * @param version the craftforJ version string, or {@code null} if unknown
   * @param licensed whether a valid license is present
   * @param capabilities the list of supported capability keys
   * @param compileGate the compile gate level for free form source changes, or {@code null}
   */
  public GetCapabilitiesAction(String version, boolean licensed, List<String> capabilities,
      String compileGate) {
    this.version = version;
    this.licensed = licensed;
    this.capabilities = capabilities;
    this.compileGate = compileGate;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getAction() {
    return ACTION;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Response handle(JsonObject params) {
    return new Response(version, licensed, capabilities, compileGate);
  }

  /**
   * Response containing server version and capabilities.
   *
   * @author Hyyan Abo Fakher
   * @since 26.02
   */
  public static class Response {

    private final String version;
    private final boolean licensed;
    private final List<String> capabilities;
    private final String compileGate;

    /**
     * Creates a new response.
     *
     * @param version the craftforJ version string
     * @param licensed whether a valid license is present
     * @param capabilities the list of supported capability keys
     * @param compileGate the compile gate level for free form source changes, or {@code null}
     */
    Response(String version, boolean licensed, List<String> capabilities, String compileGate) {
      this.version = version;
      this.licensed = licensed;
      this.capabilities = capabilities;
      this.compileGate = compileGate;
    }

    /**
     * Gets the craftforJ version.
     *
     * @return the version string, or {@code null} if unknown
     */
    public String getVersion() {
      return version;
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
     * Gets the supported capabilities.
     *
     * @return the list of capability keys
     */
    public List<String> getCapabilities() {
      return capabilities;
    }

    /**
     * Gets the compile gate level for free form source changes.
     *
     * @return the compile gate level, or {@code null} when the feature is absent
     */
    public String getCompileGate() {
      return compileGate;
    }
  }
}
