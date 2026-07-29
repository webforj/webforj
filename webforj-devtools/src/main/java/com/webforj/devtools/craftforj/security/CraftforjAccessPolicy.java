package com.webforj.devtools.craftforj.security;

import com.google.common.net.InetAddresses;
import com.typesafe.config.Config;
import com.typesafe.config.ConfigValue;
import com.typesafe.config.ConfigValueType;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/**
 * Decides whether craftforJ may attach to the page being served.
 *
 * <p>
 * craftforJ reads and writes project sources, so it stays off unless the browser asking for it sits
 * on the machine that runs the application. The loopback address is always allowed. Any other
 * client has to be named in {@value #KEY_HOSTS_ALLOWED}, a list of addresses where an entry may end
 * in {@code *} to cover a prefix, or a single {@code *} entry to drop the check entirely. A plain
 * string is read as a one entry list.
 * </p>
 *
 * <p>
 * craftforJ is opt in. {@value #KEY_ENABLED} has to be set for the panel to attach, and debug mode
 * has to be on as well, so neither one alone is enough.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class CraftforjAccessPolicy {

  /**
   * The configuration key that turns craftforJ on.
   */
  public static final String KEY_ENABLED = "webforj.devtools.craftforj.enabled";

  /**
   * The configuration key listing the client addresses allowed beyond loopback.
   */
  public static final String KEY_HOSTS_ALLOWED = "webforj.devtools.craftforj.hosts-allowed";

  private static final String WILDCARD = "*";
  private static final String LOCALHOST = "localhost";

  private CraftforjAccessPolicy() {}

  /**
   * Decides whether craftforJ may attach for the given client.
   *
   * @param config the webforJ configuration, may be {@code null}
   * @param clientAddress the address of the client the page is served to, may be {@code null}
   * @return {@code true} when craftforJ may attach
   */
  public static boolean isAllowed(Config config, String clientAddress) {
    return isEnabled(config) && isAllowedHost(config, clientAddress);
  }

  /**
   * Indicates whether craftforJ is turned on in the configuration.
   *
   * @param config the webforJ configuration, may be {@code null}
   * @return {@code true} when craftforJ is on
   */
  public static boolean isEnabled(Config config) {
    if (config == null || !config.hasPath(KEY_ENABLED) || config.getIsNull(KEY_ENABLED)) {
      return false;
    }

    return config.getBoolean(KEY_ENABLED);
  }

  /**
   * Indicates whether the given client address may reach craftforJ.
   *
   * @param config the webforJ configuration, may be {@code null}
   * @param clientAddress the address of the client the page is served to, may be {@code null}
   * @return {@code true} when the address is loopback or covered by the allow list
   */
  public static boolean isAllowedHost(Config config, String clientAddress) {
    List<String> allowed = readAllowedHosts(config);
    if (allowed.contains(WILDCARD)) {
      return true;
    }

    String address = normalize(clientAddress);
    if (address == null) {
      return false;
    }

    if (isLoopback(address)) {
      return true;
    }

    return allowed.stream().anyMatch(entry -> covers(entry, address));
  }

  /**
   * Indicates whether the given address is a loopback address.
   *
   * <p>
   * The address is read as a literal, never resolved, so a hostile value cannot turn the check into
   * a name lookup. {@code localhost} is the one name accepted, since a browser on the same machine
   * reports it.
   * </p>
   *
   * @param address the normalized address
   * @return {@code true} for a loopback address
   */
  static boolean isLoopback(String address) {
    if (LOCALHOST.equals(address)) {
      return true;
    }

    if (!InetAddresses.isInetAddress(address)) {
      return false;
    }

    return InetAddresses.forString(address).isLoopbackAddress();
  }

  private static boolean covers(String entry, String address) {
    if (entry.endsWith(WILDCARD)) {
      return address.startsWith(entry.substring(0, entry.length() - 1));
    }

    return entry.equals(address);
  }

  private static List<String> readAllowedHosts(Config config) {
    if (config == null || !config.hasPath(KEY_HOSTS_ALLOWED)) {
      return List.of();
    }

    ConfigValue value = config.getValue(KEY_HOSTS_ALLOWED);
    List<String> raw =
        value.valueType() == ConfigValueType.LIST ? config.getStringList(KEY_HOSTS_ALLOWED)
            : List.of(config.getString(KEY_HOSTS_ALLOWED));

    List<String> entries = new ArrayList<>();
    for (String entry : raw) {
      String normalized = normalize(entry);
      if (normalized != null) {
        entries.add(normalized);
      }
    }

    return entries;
  }

  private static String normalize(String address) {
    if (address == null) {
      return null;
    }

    String normalized = address.trim().toLowerCase(Locale.ROOT);
    if (normalized.startsWith("[") && normalized.endsWith("]")) {
      normalized = normalized.substring(1, normalized.length() - 1);
    }

    int zone = normalized.indexOf('%');
    if (zone >= 0) {
      normalized = normalized.substring(0, zone);
    }

    return normalized.isEmpty() ? null : normalized;
  }
}
