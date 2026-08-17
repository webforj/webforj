package com.webforj.mcp;

import io.modelcontextprotocol.spec.McpSchema.CallToolResult;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.HexFormat;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import tools.jackson.databind.JsonNode;

/**
 * Connects the tool calls of an MCP session to the running application the session opened.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
final class McpAppInstances {

  static final String INSTANCE_META_KEY = "webforj/instance";
  private static final SecureRandom RANDOM = new SecureRandom();
  private static final byte[] TOKEN_SALT = createTokenSalt();
  private static final Map<String, McpHost> instancesByToken = new ConcurrentHashMap<>();

  private McpAppInstances() {}

  static String deriveToken(String sessionId, String appToolName) {
    MessageDigest digest = createDigest();
    digest.update(TOKEN_SALT);
    digest.update(sessionId.getBytes(StandardCharsets.UTF_8));
    digest.update((byte) 0);

    return HexFormat.of().formatHex(digest.digest(appToolName.getBytes(StandardCharsets.UTF_8)));
  }

  static void bindInstance(String token, McpHost host) {
    instancesByToken.put(token, host);
  }

  static void unbindInstance(String token, McpHost host) {
    instancesByToken.remove(token, host);
  }

  static CallToolResult answerUpdateCall(String sessionId, String appToolName, JsonNode arguments) {
    McpHost host = instancesByToken.get(deriveToken(sessionId, appToolName));
    if (host == null) {
      return createNotOpenResponse(appToolName);
    }

    return host.answerToolCall(appToolName, arguments);
  }

  static CallToolResult answerActionCall(String sessionId, String appToolName,
      McpAppActionDescriptor action, JsonNode input) {
    McpHost host = instancesByToken.get(deriveToken(sessionId, appToolName));
    if (host == null) {
      return createNotOpenResponse(appToolName);
    }

    return host.answerActionCall(appToolName, action, input);
  }

  private static CallToolResult createNotOpenResponse(String appToolName) {
    return CallToolResult.builder().isError(true)
        .addTextContent("The view '" + appToolName
            + "' is not open in this conversation. Call the tool '" + appToolName + "' to open it.")
        .build();
  }

  private static MessageDigest createDigest() {
    try {
      return MessageDigest.getInstance("SHA-256");
    } catch (NoSuchAlgorithmException e) {
      throw new IllegalStateException("The platform offers no SHA-256 digest", e);
    }
  }

  private static byte[] createTokenSalt() {
    byte[] salt = new byte[32];
    RANDOM.nextBytes(salt);

    return salt;
  }
}
