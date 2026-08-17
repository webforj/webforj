package com.webforj.mcp;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.util.List;
import java.util.Map;

/**
 * The capabilities an MCP host reported in the handshake.
 *
 * @param experimental the experimental capabilities of the host
 * @param openLinks present when the host opens links for the application
 * @param downloadFile present when the host downloads files for the application
 * @param serverTools the tool support of the host
 * @param serverResources the resource support of the host
 * @param logging present when the host accepts log notifications
 * @param sandbox the sandbox the host renders the application in
 * @param updateModelContext the content blocks the host accepts as model context updates
 * @param message the content blocks the host accepts as conversation messages
 * @param sampling the sampling support of the host
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
// @formatter:off
@JsonIgnoreProperties(ignoreUnknown = true)
public record McpHostCapabilities(
    Map<String, Object> experimental,
    Map<String, Object> openLinks,
    Map<String, Object> downloadFile,
    ServerTools serverTools,
    ServerResources serverResources,
    Map<String, Object> logging,
    Sandbox sandbox,
    ContentSupport updateModelContext,
    ContentSupport message,
    Sampling sampling) {

  /**
   * The tool support of the host.
   *
   * @param listChanged whether the host forwards tool list change notifications
   */
  @JsonIgnoreProperties(ignoreUnknown = true)
  public record ServerTools(Boolean listChanged) {
  }

  /**
   * The resource support of the host.
   *
   * @param listChanged whether the host forwards resource list change notifications
   */
  @JsonIgnoreProperties(ignoreUnknown = true)
  public record ServerResources(Boolean listChanged) {
  }

  /**
   * The sandbox the host renders the application in.
   *
   * @param permissions the permissions the sandbox grants
   * @param csp the content security policy the sandbox applies
   */
  @JsonIgnoreProperties(ignoreUnknown = true)
  public record Sandbox(Permissions permissions, Csp csp) {
  }

  /**
   * The permissions the sandbox grants.
   *
   * @param camera present when the sandbox grants camera access
   * @param microphone present when the sandbox grants microphone access
   * @param geolocation present when the sandbox grants geolocation access
   * @param clipboardWrite present when the sandbox grants clipboard writes
   */
  @JsonIgnoreProperties(ignoreUnknown = true)
  public record Permissions(
      Map<String, Object> camera,
      Map<String, Object> microphone,
      Map<String, Object> geolocation,
      Map<String, Object> clipboardWrite) {
  }

  /**
   * The content security policy the sandbox applies.
   *
   * @param connectDomains the origins the application can connect to
   * @param resourceDomains the origins the application can load resources from
   * @param frameDomains the origins the application can embed in frames
   * @param baseUriDomains the origins allowed as the document base
   */
  @JsonIgnoreProperties(ignoreUnknown = true)
  public record Csp(
      List<String> connectDomains,
      List<String> resourceDomains,
      List<String> frameDomains,
      List<String> baseUriDomains) {
  }

  /**
   * The content blocks the host accepts on a channel.
   *
   * @param text present when the host accepts text blocks
   * @param image present when the host accepts image blocks
   * @param audio present when the host accepts audio blocks
   * @param resource present when the host accepts resource blocks
   * @param resourceLink present when the host accepts resource link blocks
   */
  @JsonIgnoreProperties(ignoreUnknown = true)
  public record ContentSupport(
      Map<String, Object> text,
      Map<String, Object> image,
      Map<String, Object> audio,
      Map<String, Object> resource,
      Map<String, Object> resourceLink) {
  }

  /**
   * The sampling support of the host.
   *
   * @param tools present when the host samples with tool use
   */
  @JsonIgnoreProperties(ignoreUnknown = true)
  public record Sampling(Map<String, Object> tools) {
  }
}
// @formatter:on
