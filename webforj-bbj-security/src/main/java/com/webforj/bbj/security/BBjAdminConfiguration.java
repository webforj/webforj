package com.webforj.bbj.security;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigResolveOptions;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.net.InetAddress;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.UnknownHostException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Locale;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The Admin API connection and token settings of an app running in BBjServices, read from the
 * {@code webforj.security.bbj} configuration keys.
 *
 * <p>
 * Supported keys are {@code admin.host}, {@code admin.port}, {@code admin.ssl},
 * {@code admin.truststore}, {@code admin.truststore-password}, {@code admin.timeout-seconds} and
 * {@code token.minutes}. A key below {@code webforj.security.bbj} that is not supported is logged
 * once as a warning, naming the key and never the value, and is ignored. The defaults are
 * {@code 127.0.0.1}, {@code 2002}, {@code false}, no truststore, an empty password, 10 seconds and
 * 30 minutes. A key that is not set, or is set to {@code null}, uses its default and is logged at
 * debug level.
 * </p>
 *
 * <p>
 * The connection keys decide which server answers the logins. An invalid value of a connection key
 * stops the creation with an {@link IllegalArgumentException} that names the key. A host that is
 * not a loopback address requires {@code admin.ssl} to be {@code true} and {@code admin.truststore}
 * to be set. With {@code admin.ssl} set to {@code true}, {@code admin.truststore} must name an
 * existing, regular and readable file. The message names the key that failed and never a value or a
 * path. A truststore is ignored with a warning while {@code admin.ssl} is {@code false}.
 * </p>
 *
 * <p>
 * An invalid value of {@code admin.timeout-seconds} (1 to 300) or {@code token.minutes} (1 to 480)
 * is logged once as a warning and uses the default. A {@code token.minutes} above 480 is lowered to
 * 480, the longest token lifetime of the server.
 * </p>
 *
 * <p>
 * Values and secrets are never logged, and the password is not part of {@link #toString()}.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjAdminConfiguration {
  private static final String PREFIX = "webforj.security.bbj.";
  private static final String HOST = PREFIX + "admin.host";
  private static final String PORT = PREFIX + "admin.port";
  private static final String SSL = PREFIX + "admin.ssl";
  private static final String TRUSTSTORE = PREFIX + "admin.truststore";
  private static final String TRUSTSTORE_PASSWORD = PREFIX + "admin.truststore-password";
  private static final String TIMEOUT_SECONDS = PREFIX + "admin.timeout-seconds";
  private static final String TOKEN_MINUTES = PREFIX + "token.minutes";
  private static final Set<String> KNOWN_KEYS =
      Set.of(HOST, PORT, SSL, TRUSTSTORE, TRUSTSTORE_PASSWORD, TIMEOUT_SECONDS, TOKEN_MINUTES);
  private static final String DEFAULT_HOST = "127.0.0.1";
  private static final int DEFAULT_PORT = 2002;
  private static final int DEFAULT_TIMEOUT_SECONDS = 10;
  private static final int MAX_TIMEOUT_SECONDS = 300;
  private static final int DEFAULT_TOKEN_MINUTES = 30;
  private static final int MAX_TOKEN_MINUTES = 480;
  private static final Pattern IPV4_LITERAL = Pattern
      .compile("(0|[1-9]\\d{0,2})\\.(0|[1-9]\\d{0,2})\\.(0|[1-9]\\d{0,2})\\.(0|[1-9]\\d{0,2})");
  private static final Pattern IPV6_CHARACTERS = Pattern.compile("[0-9A-Fa-f:.]+");
  private static final Logger logger = System.getLogger(BBjAdminConfiguration.class.getName());

  private final String host;
  private final int port;
  private final boolean ssl;
  private final Path truststore;
  private final String truststorePassword;
  private final Duration timeout;
  private final Duration tokenLifetime;

  private BBjAdminConfiguration(Builder builder) {
    this.host = builder.host;
    this.port = builder.port;
    this.ssl = builder.ssl;
    this.truststore = builder.truststore;
    this.truststorePassword = builder.truststorePassword;
    this.timeout = builder.timeout;
    this.tokenLifetime = builder.tokenLifetime;
  }

  /**
   * Describes the connection without the truststore path and password.
   *
   * @return the host, port, TLS, truststore, timeout and token lifetime
   */
  @Override
  public String toString() {
    return host + ":" + port + " ssl=" + ssl + " truststore="
        + (truststore == null ? "none" : "set") + " timeout=" + timeout.toSeconds() + "s token="
        + tokenLifetime.toMinutes() + "min";
  }

  /**
   * Gets the host of the Admin API.
   *
   * @return the host, trimmed
   */
  String getHost() {
    return host;
  }

  /**
   * Gets the port of the Admin API.
   *
   * @return the port
   */
  int getPort() {
    return port;
  }

  /**
   * Checks whether the connection to the Admin API uses TLS.
   *
   * @return {@code true} when TLS is used
   */
  boolean isSsl() {
    return ssl;
  }

  /**
   * Gets the truststore that authenticates the Admin API server.
   *
   * @return the truststore, or empty when none is used
   */
  Optional<Path> getTruststore() {
    return Optional.ofNullable(truststore);
  }

  /**
   * Gets the password of the truststore.
   *
   * @return the password, empty when none is set
   */
  String getTruststorePassword() {
    return truststorePassword;
  }

  /**
   * Gets the time one Admin API operation may take.
   *
   * @return the timeout
   */
  Duration getTimeout() {
    return timeout;
  }

  /**
   * Gets the lifetime of a login token.
   *
   * @return the lifetime
   */
  Duration getTokenLifetime() {
    return tokenLifetime;
  }

  /**
   * Reads the configuration from the given config.
   *
   * <p>
   * Optional substitutions such as {@code ${?VAR}} are resolved from the environment of the
   * process. A substitution that cannot be resolved makes only its own key invalid.
   * </p>
   *
   * @param config the config to read
   * @return the configuration
   *
   * @throws IllegalArgumentException when a connection key is invalid or the host is not a loopback
   *         address and the connection is not secured with a truststore
   */
  static BBjAdminConfiguration create(Config config) {
    final BBjConfigReader reader = new BBjConfigReader(resolveSubstitutions(config), logger);
    reader.warnUnknownKeys(PREFIX.substring(0, PREFIX.length() - 1), KNOWN_KEYS);
    final Builder builder = new Builder();
    builder.host = reader
        .requireString(HOST, DEFAULT_HOST, BBjAdminConfiguration::isHost,
            "a host name or IP address without scheme, port or path")
        .map(String::trim).orElse(DEFAULT_HOST);
    builder.port = reader.requireInt(PORT, DEFAULT_PORT, 1, 65535);
    builder.ssl = reader.requireBoolean(SSL, false);
    builder.truststorePassword =
        reader.requireString(TRUSTSTORE_PASSWORD, "empty", password -> true, "a text").orElse("");
    builder.timeout = Duration.ofSeconds(
        reader.readInt(TIMEOUT_SECONDS, DEFAULT_TIMEOUT_SECONDS, 1, MAX_TIMEOUT_SECONDS));
    builder.tokenLifetime = Duration.ofMinutes(
        reader.readClampedInt(TOKEN_MINUTES, DEFAULT_TOKEN_MINUTES, 1, MAX_TOKEN_MINUTES));

    Optional<String> truststorePath =
        reader.requireString(TRUSTSTORE, "none", BBjAdminConfiguration::isPath, "a path");

    if (builder.ssl && truststorePath.isPresent()) {
      findTruststoreProblem(truststorePath.get()).ifPresent(problem -> {
        throw new IllegalArgumentException(problem);
      });
    }

    if (!isLoopback(builder.host)) {
      if (!builder.ssl) {
        throw new IllegalArgumentException(
            "The host in " + HOST + " is not a loopback address, so " + SSL + " must be true");
      }

      if (truststorePath.isEmpty()) {
        throw new IllegalArgumentException("The host in " + HOST + " is not a loopback address, so "
            + TRUSTSTORE + " must be set");
      }
    }

    if (builder.ssl) {
      builder.truststore = truststorePath.map(Path::of).orElse(null);
    } else if (truststorePath.isPresent()) {
      logger.log(Level.WARNING, "{0} is ignored while {1} is false", TRUSTSTORE, SSL);
    }

    return new BBjAdminConfiguration(builder);
  }

  private static Config resolveSubstitutions(Config config) {
    return config.resolve(ConfigResolveOptions.defaults().setAllowUnresolved(true));
  }

  private static boolean isHost(String host) {
    String trimmed = host.trim();
    if (trimmed.isEmpty() || trimmed.indexOf('/') >= 0
        || trimmed.chars().anyMatch(Character::isWhitespace)) {
      return false;
    }

    return trimmed.indexOf(':') < 0 || isIpv6Literal(trimmed);
  }

  private static boolean isIpv6Literal(String host) {
    if (!IPV6_CHARACTERS.matcher(host).matches()) {
      return false;
    }

    try {
      new URI("http://[" + host + "]/");
      return true;
    } catch (URISyntaxException e) {
      return false;
    }
  }

  private static boolean isLoopback(String host) {
    if (host.indexOf(':') >= 0) {
      try {
        return InetAddress.getByName(host).isLoopbackAddress();
      } catch (UnknownHostException e) {
        return false;
      }
    }

    Matcher matcher = IPV4_LITERAL.matcher(host);
    if (matcher.matches()) {
      return Integer.parseInt(matcher.group(1)) == 127 && isOctet(matcher.group(2))
          && isOctet(matcher.group(3)) && isOctet(matcher.group(4));
    }

    return host.toLowerCase(Locale.ROOT).equals("localhost");
  }

  private static boolean isOctet(String text) {
    return Integer.parseInt(text) <= 255;
  }



  private static Optional<String> findTruststoreProblem(String path) {
    Path file = Path.of(path);
    if (!Files.exists(file)) {
      return Optional.of(TRUSTSTORE + " must name an existing file");
    }

    if (!Files.isRegularFile(file)) {
      return Optional.of(TRUSTSTORE + " must name a regular file");
    }

    return Files.isReadable(file) ? Optional.empty()
        : Optional.of(TRUSTSTORE + " must name a file that can be read");
  }

  private static boolean isPath(String path) {
    try {
      Path.of(path);
      return !path.isBlank();
    } catch (InvalidPathException e) {
      return false;
    }
  }

  private static final class Builder {
    String host;
    int port;
    boolean ssl;
    Path truststore;
    String truststorePassword;
    Duration timeout;
    Duration tokenLifetime;
  }
}
