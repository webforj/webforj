package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import com.webforj.bbj.security.fixture.CapturedLog;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Optional;
import java.util.logging.Level;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

class BBjAdminConfigurationTest {
  private static final String PREFIX = "webforj.security.bbj.";
  private static final String HOST = PREFIX + "admin.host";
  private static final String PORT = PREFIX + "admin.port";
  private static final String SSL = PREFIX + "admin.ssl";
  private static final String TRUSTSTORE = PREFIX + "admin.truststore";
  private static final String PASSWORD = PREFIX + "admin.truststore-password";
  private static final String TIMEOUT = PREFIX + "admin.timeout-seconds";
  private static final String TOKEN_MINUTES = PREFIX + "token.minutes";
  private static final String STORE_MAX_ENTRIES = PREFIX + "token.store-max-entries";
  private CapturedLog log;

  @BeforeEach
  void setUp() {
    log = new CapturedLog(BBjAdminConfiguration.class.getName());
  }

  @AfterEach
  void tearDown() {
    log.close();
  }

  private static BBjAdminConfiguration createConfiguration(String... lines) {
    return BBjAdminConfiguration.create(createConfig(lines));
  }

  private static Config createConfig(String... lines) {
    return ConfigFactory.parseString(String.join("\n", lines));
  }

  @Test
  void shouldUseTheDefaultsForAnEmptyConfig() {
    BBjAdminConfiguration configuration = BBjAdminConfiguration.create(ConfigFactory.empty());

    assertEquals("127.0.0.1", configuration.getHost());
    assertEquals(2002, configuration.getPort());
    assertFalse(configuration.isSsl());
    assertEquals(Optional.empty(), configuration.getTruststore());
    assertEquals("", configuration.getTruststorePassword());
    assertEquals(Duration.ofSeconds(10), configuration.getTimeout());
    assertEquals(Duration.ofMinutes(30), configuration.getTokenLifetime());
    assertTrue(log.getMessages(Level.WARNING).isEmpty());
    assertEquals(
        List.of(HOST + " is not set, using 127.0.0.1", PORT + " is not set, using 2002",
            SSL + " is not set, using false", PASSWORD + " is not set, using empty",
            TIMEOUT + " is not set, using 10", TOKEN_MINUTES + " is not set, using 30",
            STORE_MAX_ENTRIES + " is not set, using 10000", TRUSTSTORE + " is not set, using none"),
        log.getMessages(Level.FINE));
  }

  @Test
  void shouldTreatNullValuesAsMissing() {
    BBjAdminConfiguration configuration = createConfiguration(HOST + " = null", PORT + " = null",
        SSL + " = null", TRUSTSTORE + " = null", PASSWORD + " = null", TIMEOUT + " = null",
        TOKEN_MINUTES + " = null", STORE_MAX_ENTRIES + " = null");

    assertEquals("127.0.0.1", configuration.getHost());
    assertEquals(2002, configuration.getPort());
    assertEquals(Duration.ofSeconds(10), configuration.getTimeout());
    assertEquals(Duration.ofMinutes(30), configuration.getTokenLifetime());
    assertEquals(8, log.getMessages(Level.FINE).size());
    assertTrue(log.getMessages(Level.WARNING).isEmpty());
  }

  @Nested
  class ValidValues {
    @Test
    void shouldReadTheHostTrimmed() {
      assertEquals("localhost", createConfiguration(HOST + " = \"  localhost \"").getHost());
    }

    @Test
    void shouldReadThePort() {
      assertEquals(2003, createConfiguration(PORT + " = 2003").getPort());
      assertEquals(1, createConfiguration(PORT + " = 1").getPort());
      assertEquals(65535, createConfiguration(PORT + " = 65535").getPort());
    }

    @Test
    void shouldReadTheSsl() {
      assertTrue(createConfiguration(SSL + " = true").isSsl());
    }

    @Test
    void shouldReadTheTruststoreAndPassword(@TempDir Path folder) throws IOException {
      Path file = Files.createFile(folder.resolve("store.jks"));

      BBjAdminConfiguration configuration = createConfiguration(SSL + " = true",
          TRUSTSTORE + " = \"" + file + "\"", PASSWORD + " = changeit");

      assertEquals(Optional.of(file), configuration.getTruststore());
      assertEquals("changeit", configuration.getTruststorePassword());
    }

    @Test
    void shouldReadTheTimeout() {
      assertEquals(Duration.ofSeconds(1), createConfiguration(TIMEOUT + " = 1").getTimeout());
      assertEquals(Duration.ofSeconds(300), createConfiguration(TIMEOUT + " = 300").getTimeout());
      assertEquals(Duration.ofSeconds(45), createConfiguration(TIMEOUT + " = 45").getTimeout());
      assertTrue(log.getMessages(Level.WARNING).isEmpty());
    }

    @Test
    void shouldReadTheTokenMinutes() {
      assertEquals(Duration.ofMinutes(1),
          createConfiguration(TOKEN_MINUTES + " = 1").getTokenLifetime());
      assertEquals(Duration.ofMinutes(480),
          createConfiguration(TOKEN_MINUTES + " = 480").getTokenLifetime());
      assertEquals(Duration.ofMinutes(60),
          createConfiguration(TOKEN_MINUTES + " = 60").getTokenLifetime());
      assertTrue(log.getMessages(Level.WARNING).isEmpty());
    }
  }

  @Nested
  class TokenStoreMaxEntries {
    static Stream<String> invalidValues() {
      return Stream.of("0", "-1", "100001", "1.5", "\"notANumberValue\"", "${nowhere.nothing}");
    }

    @Test
    void shouldDefaultToTenThousand() {
      assertEquals(10000,
          BBjAdminConfiguration.create(ConfigFactory.empty()).getTokenStoreMaxEntries());
    }

    @ParameterizedTest
    @ValueSource(strings = {"1", "250", "100000"})
    void shouldReadValidValue(String value) {
      BBjAdminConfiguration configuration = createConfiguration(STORE_MAX_ENTRIES + " = " + value);

      assertEquals(Integer.parseInt(value), configuration.getTokenStoreMaxEntries());
      assertTrue(log.getMessages(Level.WARNING).isEmpty());
    }

    @ParameterizedTest
    @MethodSource("invalidValues")
    void shouldUseTheDefaultAndWarnOnceNamingTheKeyButNotTheValue(String value) {
      BBjAdminConfiguration configuration = createConfiguration(STORE_MAX_ENTRIES + " = " + value);

      assertEquals(10000, configuration.getTokenStoreMaxEntries());
      List<String> warnings = log.getMessages(Level.WARNING);
      assertEquals(1, warnings.size());
      assertTrue(warnings.get(0).contains(STORE_MAX_ENTRIES));
      assertFalse(warnings.get(0).contains("notA"));
    }

    @Test
    void shouldNotWarnAboutTheKeyAsUnknown() {
      createConfiguration(STORE_MAX_ENTRIES + " = 20");

      assertTrue(log.getAllMessages().stream().noneMatch(text -> text.contains("not a known")));
    }
  }

  @Nested
  class InvalidConnectionKeys {
    static Stream<Arguments> invalidValues() {
      return Stream.of(Arguments.of(PORT, "0"), Arguments.of(PORT, "65536"),
          Arguments.of(PORT, "\"notANumberValue\""), Arguments.of(PORT, "1.5"),
          Arguments.of(PORT, "-1"), Arguments.of(HOST, "\"\""), Arguments.of(HOST, "\"   \""),
          Arguments.of(HOST, "\"https://notAHostValue\""), Arguments.of(HOST, "\"not AHost\""),
          Arguments.of(HOST, "\"notAHost/path\""), Arguments.of(HOST, "\"notAHost:80\""),
          Arguments.of(HOST, "\"1.2.3.4:80\""), Arguments.of(HOST, "\"[::1\""),
          Arguments.of(HOST, "[1]"), Arguments.of(SSL, "\"notABooleanValue\""),
          Arguments.of(TRUSTSTORE, "\"\""), Arguments.of(PASSWORD, "[1]"));
    }

    @ParameterizedTest
    @MethodSource("invalidValues")
    void shouldThrowNamingTheKeyButNotTheValue(String key, String value) {
      Config config = createConfig(key + " = " + value);

      IllegalArgumentException exception =
          assertThrows(IllegalArgumentException.class, () -> BBjAdminConfiguration.create(config));

      assertTrue(exception.getMessage().contains(key));
      assertFalse(exception.getMessage().contains("notA"));
      assertTrue(log.getAllMessages().stream().noneMatch(text -> text.contains("notA")));
    }

    @Test
    void shouldThrowForTruststoreThatIsNotReadableFile(@TempDir Path folder) {
      Config missing = createConfig(SSL + " = true",
          TRUSTSTORE + " = \"" + folder.resolve("missing.jks") + "\"");
      Config directory = createConfig(SSL + " = true", TRUSTSTORE + " = \"" + folder + "\"");

      assertTrue(
          assertThrows(IllegalArgumentException.class, () -> BBjAdminConfiguration.create(missing))
              .getMessage().contains(TRUSTSTORE));
      assertTrue(assertThrows(IllegalArgumentException.class,
          () -> BBjAdminConfiguration.create(directory)).getMessage().contains(TRUSTSTORE));
    }

    @Test
    void shouldThrowForTruststoreWithoutReadPermission(@TempDir Path folder) throws IOException {
      Path file = Files.createFile(folder.resolve("locked.jks"));
      assumeTrue(file.toFile().setReadable(false) && !Files.isReadable(file));
      Config config = createConfig(SSL + " = true", TRUSTSTORE + " = \"" + file + "\"");

      assertThrows(IllegalArgumentException.class, () -> BBjAdminConfiguration.create(config));
    }

    @Test
    void shouldThrowForTruststorePathThatIsNotPath() {
      Config config = createConfig(SSL + " = true", TRUSTSTORE + " = \"bad\\u0000path\"");

      assertThrows(IllegalArgumentException.class, () -> BBjAdminConfiguration.create(config));
    }

    @Test
    void shouldThrowForAnUnresolvedRequiredSubstitution() {
      Config config = createConfig(PASSWORD + " = ${nowhere.nothing}");

      IllegalArgumentException exception =
          assertThrows(IllegalArgumentException.class, () -> BBjAdminConfiguration.create(config));

      assertTrue(exception.getMessage().contains(PASSWORD));
    }
  }

  @Nested
  class InvalidTuningKeys {
    static Stream<Arguments> invalidValues() {
      return Stream.of(Arguments.of(TIMEOUT, "0", Duration.ofSeconds(10)),
          Arguments.of(TIMEOUT, "301", Duration.ofSeconds(10)),
          Arguments.of(TIMEOUT, "-3", Duration.ofSeconds(10)),
          Arguments.of(TIMEOUT, "1.5", Duration.ofSeconds(10)),
          Arguments.of(TIMEOUT, "\"notANumberValue\"", Duration.ofSeconds(10)),
          Arguments.of(TIMEOUT, "${nowhere.nothing}", Duration.ofSeconds(10)),
          Arguments.of(TOKEN_MINUTES, "0", Duration.ofMinutes(30)),
          Arguments.of(TOKEN_MINUTES, "-5", Duration.ofMinutes(30)),
          Arguments.of(TOKEN_MINUTES, "2.5", Duration.ofMinutes(30)),
          Arguments.of(TOKEN_MINUTES, "\"notANumberValue\"", Duration.ofMinutes(30)),
          Arguments.of(TOKEN_MINUTES, "${nowhere.nothing}", Duration.ofMinutes(30)));
    }

    @ParameterizedTest
    @MethodSource("invalidValues")
    void shouldUseTheDefaultAndWarnOnceNamingTheKeyButNotTheValue(String key, String value,
        Duration expected) {
      BBjAdminConfiguration configuration = createConfiguration(key + " = " + value);

      Duration actual =
          key.equals(TIMEOUT) ? configuration.getTimeout() : configuration.getTokenLifetime();
      assertEquals(expected, actual);
      List<String> warnings = log.getMessages(Level.WARNING);
      assertEquals(1, warnings.size());
      assertTrue(warnings.get(0).contains(key));
      assertFalse(warnings.get(0).contains("notA"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"481", "1441", "100000"})
    void shouldClampTheTokenMinutesToTheServerMaximumAndWarnOnce(String value) {
      BBjAdminConfiguration configuration = createConfiguration(TOKEN_MINUTES + " = " + value);

      assertEquals(Duration.ofMinutes(480), configuration.getTokenLifetime());
      assertEquals(List.of("Value of " + TOKEN_MINUTES + " is above 480, using 480"),
          log.getMessages(Level.WARNING));
    }
  }

  @Nested
  class RemoteHost {
    @ParameterizedTest
    @ValueSource(strings = {"127.0.0.1", "127.0.0.2", "127.255.255.255", "::1", "0:0:0:0:0:0:0:1",
        "localhost", "LOCALHOST"})
    void shouldAcceptLoopbackHostWithoutTls(String host) {
      assertEquals(host, createConfiguration(HOST + " = \"" + host + "\"").getHost());
    }

    @ParameterizedTest
    @ValueSource(strings = {"bbj.example.com", "localhost.example.com", "128.0.0.1", "10.0.0.5",
        "126.0.0.1", "::2", "2001:db8::1", "127.0.0.1.example.com", "0127.0.0.1", "127.0.0.256"})
    void shouldRefuseRemoteHostWithoutTls(String host) {
      Config config = createConfig(HOST + " = \"" + host + "\"");

      IllegalArgumentException exception =
          assertThrows(IllegalArgumentException.class, () -> BBjAdminConfiguration.create(config));

      assertEquals(
          "The host in " + HOST + " is not a loopback address, so " + SSL + " must be true",
          exception.getMessage());
    }

    @Test
    void shouldRefuseRemoteHostWithTlsButWithoutTruststore() {
      Config config = createConfig(HOST + " = \"bbj.example.com\"", SSL + " = true");

      IllegalArgumentException exception =
          assertThrows(IllegalArgumentException.class, () -> BBjAdminConfiguration.create(config));

      assertEquals(
          "The host in " + HOST + " is not a loopback address, so " + TRUSTSTORE + " must be set",
          exception.getMessage());
      assertNull(exception.getCause());
    }

    @Test
    void shouldRefuseRemoteHostWithTruststoreButWithoutTls(@TempDir Path folder)
        throws IOException {
      Path file = Files.createFile(folder.resolve("store.jks"));
      Config config =
          createConfig(HOST + " = \"bbj.example.com\"", TRUSTSTORE + " = \"" + file + "\"");

      IllegalArgumentException exception =
          assertThrows(IllegalArgumentException.class, () -> BBjAdminConfiguration.create(config));

      assertEquals(
          "The host in " + HOST + " is not a loopback address, so " + SSL + " must be true",
          exception.getMessage());
      assertFalse(exception.getMessage().contains(file.toString()));
    }

    @Test
    void shouldAcceptRemoteHostWithTlsAndTruststore(@TempDir Path folder) throws IOException {
      Path file = Files.createFile(folder.resolve("store.jks"));

      BBjAdminConfiguration configuration = createConfiguration(HOST + " = \"bbj.example.com\"",
          SSL + " = true", TRUSTSTORE + " = \"" + file + "\"");

      assertEquals("bbj.example.com", configuration.getHost());
      assertTrue(configuration.isSsl());
    }

    @Test
    void shouldAcceptLoopbackHostWithTlsAndWithoutTruststore() {
      BBjAdminConfiguration configuration = createConfiguration(SSL + " = true");

      assertTrue(configuration.isSsl());
      assertEquals(Optional.empty(), configuration.getTruststore());
      assertTrue(log.getMessages(Level.WARNING).isEmpty());
    }

    @Test
    void shouldIgnoreTruststoreWithoutTlsOnLoopbackAndWarnOnce() {
      BBjAdminConfiguration configuration =
          createConfiguration(TRUSTSTORE + " = \"/does/not/exist.jks\"");

      assertEquals(Optional.empty(), configuration.getTruststore());
      assertEquals(List.of(TRUSTSTORE + " is ignored while " + SSL + " is false"),
          log.getMessages(Level.WARNING));
    }
  }

  @Nested
  class Substitutions {
    @Test
    void shouldReadTheEnvironmentForAnOptionalSubstitution() {
      assumeTrue(System.getenv("PATH") != null);

      BBjAdminConfiguration configuration = createConfiguration(PASSWORD + " = ${?PATH}");

      assertEquals(System.getenv("PATH"), configuration.getTruststorePassword());
    }

    @Test
    void shouldTreatAnOptionalSubstitutionWithoutVariableAsMissing() {
      BBjAdminConfiguration configuration =
          createConfiguration(PASSWORD + " = ${?BBJ_SECURITY_TEST_UNSET_VARIABLE}");

      assertEquals("", configuration.getTruststorePassword());
      assertTrue(log.getMessages(Level.FINE).contains(PASSWORD + " is not set, using empty"));
      assertTrue(log.getMessages(Level.WARNING).isEmpty());
    }

    @Test
    void shouldResolveSubstitutionOfAnotherKey() {
      BBjAdminConfiguration configuration =
          createConfiguration(PORT + " = ${my.port}", "my.port = 2003");

      assertEquals(2003, configuration.getPort());
    }

    @Test
    void shouldNotFailForAnUnresolvedSubstitutionOfAnUnrelatedKey() {
      BBjAdminConfiguration configuration =
          createConfiguration("other.key = ${nowhere.nothing}", PORT + " = 2003");

      assertEquals(2003, configuration.getPort());
      assertTrue(log.getMessages(Level.WARNING).isEmpty());
    }
  }

  @Nested
  class Secrets {
    private static final String SECRET = "tsPw-9f3kSecretValue";

    @Test
    void shouldKeepThePasswordOutOfTheLogAndTheText(@TempDir Path folder) throws IOException {
      Path file = Files.createFile(folder.resolve("store.jks"));

      BBjAdminConfiguration configuration = createConfiguration(SSL + " = true",
          TRUSTSTORE + " = \"" + file + "\"", PASSWORD + " = " + SECRET, TIMEOUT + " = 0");

      assertEquals(SECRET, configuration.getTruststorePassword());
      assertFalse(configuration.toString().contains(SECRET));
      assertFalse(configuration.toString().contains(file.toString()));
      assertTrue(log.getAllMessages().stream().noneMatch(text -> text.contains(SECRET)));
    }

    @Test
    void shouldDescribeTheConnectionWithoutSecrets(@TempDir Path folder) throws IOException {
      Path file = Files.createFile(folder.resolve("store.jks"));

      assertEquals("127.0.0.1:2002 ssl=false truststore=none timeout=10s token=30min",
          createConfiguration().toString());
      assertEquals("localhost:2003 ssl=true truststore=set timeout=20s token=45min",
          createConfiguration(HOST + " = localhost", PORT + " = 2003", SSL + " = true",
              TRUSTSTORE + " = \"" + file + "\"", PASSWORD + " = " + SECRET, TIMEOUT + " = 20",
              TOKEN_MINUTES + " = 45").toString());
    }
  }

  @Nested
  class TruststoreProblems {
    @Test
    void shouldNameTheTruststoreForAMissingFile(@TempDir Path folder) {
      Path missing = folder.resolve("missing.jks");
      Config config = createConfig(SSL + " = true", TRUSTSTORE + " = \"" + missing + "\"");

      IllegalArgumentException exception =
          assertThrows(IllegalArgumentException.class, () -> BBjAdminConfiguration.create(config));

      assertEquals(TRUSTSTORE + " must name an existing file", exception.getMessage());
      assertNull(exception.getCause());
    }

    @Test
    void shouldNameTheTruststoreForAFolder(@TempDir Path folder) {
      Config config = createConfig(SSL + " = true", TRUSTSTORE + " = \"" + folder + "\"");

      IllegalArgumentException exception =
          assertThrows(IllegalArgumentException.class, () -> BBjAdminConfiguration.create(config));

      assertEquals(TRUSTSTORE + " must name a regular file", exception.getMessage());
    }

    @Test
    void shouldNameTheTruststoreForAFileThatCannotBeRead(@TempDir Path folder) throws IOException {
      Path file = Files.createFile(folder.resolve("locked.jks"));
      assumeTrue(file.toFile().setReadable(false) && !Files.isReadable(file));
      Config config = createConfig(SSL + " = true", TRUSTSTORE + " = \"" + file + "\"");

      IllegalArgumentException exception =
          assertThrows(IllegalArgumentException.class, () -> BBjAdminConfiguration.create(config));

      assertEquals(TRUSTSTORE + " must name a file that can be read", exception.getMessage());
    }

    @Test
    void shouldNameTheTruststoreForARemoteHostWithAMissingFile(@TempDir Path folder) {
      Path missing = folder.resolve("missing.jks");
      Config config = createConfig(HOST + " = \"bbj.example.com\"", SSL + " = true",
          TRUSTSTORE + " = \"" + missing + "\"");

      IllegalArgumentException exception =
          assertThrows(IllegalArgumentException.class, () -> BBjAdminConfiguration.create(config));

      assertEquals(TRUSTSTORE + " must name an existing file", exception.getMessage());
      assertFalse(exception.getMessage().contains(missing.toString()));
      assertTrue(
          log.getAllMessages().stream().noneMatch(text -> text.contains(missing.toString())));
    }

    @Test
    void shouldWarnAboutAMisspelledTruststoreKeyAndNameTheMissingOne() {
      Config config = createConfig(HOST + " = \"bbj.example.com\"", SSL + " = true",
          PREFIX + "admin.trustore = \"secretStorePath\"");

      IllegalArgumentException exception =
          assertThrows(IllegalArgumentException.class, () -> BBjAdminConfiguration.create(config));

      assertEquals(
          "The host in " + HOST + " is not a loopback address, so " + TRUSTSTORE + " must be set",
          exception.getMessage());
      assertEquals(List.of(PREFIX + "admin.trustore is not a known key and is ignored"),
          log.getMessages(Level.WARNING));
    }
  }

  @Nested
  class UnknownKeys {
    @Test
    void shouldAcceptEveryKnownKeyWithoutAWarning(@TempDir Path folder) throws IOException {
      Path file = Files.createFile(folder.resolve("store.jks"));

      createConfiguration(HOST + " = localhost", PORT + " = 2003", SSL + " = true",
          TRUSTSTORE + " = \"" + file + "\"", PASSWORD + " = x", TIMEOUT + " = 20",
          TOKEN_MINUTES + " = 45", STORE_MAX_ENTRIES + " = 500");

      assertTrue(log.getMessages(Level.WARNING).isEmpty());
    }

    @Test
    void shouldWarnOncePerUnknownKeyNamingTheFullPathButNotTheValue() {
      createConfiguration(PREFIX + "admin.foo = secretValueOne", PREFIX + "other = secretValueTwo",
          PREFIX + "admin.foo2.bar = secretValueThree", PREFIX + "admin.trustore = secretValueFour",
          PORT + " = 2003");

      assertEquals(
          List.of(PREFIX + "admin.foo is not a known key and is ignored",
              PREFIX + "admin.foo2.bar is not a known key and is ignored",
              PREFIX + "admin.trustore is not a known key and is ignored",
              PREFIX + "other is not a known key and is ignored"),
          log.getMessages(Level.WARNING).stream().sorted().toList());
      assertTrue(log.getAllMessages().stream().noneMatch(text -> text.contains("secretValue")));
    }

    @Test
    void shouldWarnForAnUnknownKeyWithAnUnresolvableSubstitution() {
      BBjAdminConfiguration configuration =
          createConfiguration(PREFIX + "admin.mystery = ${nowhere.nothing}",
              PREFIX + "admin.maybe = ${?NOWHERE_AT_ALL}", PORT + " = 2003");

      assertEquals(2003, configuration.getPort());
      assertEquals(List.of(PREFIX + "admin.mystery is not a known key and is ignored"),
          log.getMessages(Level.WARNING));
    }

    @Test
    void shouldWarnForAKeyThatIsSetToNull() {
      createConfiguration(PREFIX + "admin.nothing = null");

      assertEquals(List.of(PREFIX + "admin.nothing is not a known key and is ignored"),
          log.getMessages(Level.WARNING));
    }

    @Test
    void shouldNotWarnForKeysOutsideTheBbjObject() {
      createConfiguration("webforj.security.enabled = true", "webforj.security.other = 1",
          "somewhere.else = 2");

      assertTrue(log.getMessages(Level.WARNING).isEmpty());
    }

    @Test
    void shouldNotWarnWhenTheBbjObjectIsMissingOrNotAnObject() {
      createConfiguration();
      createConfiguration("webforj.security.bbj = \"secretValue\"");

      assertTrue(log.getAllMessages().stream().noneMatch(text -> text.contains("not a known")));
    }
  }

  @Test
  void shouldNotChangeTheConfigAndGiveEqualResultsWhenCalledTwice() {
    Config config = createConfig(PORT + " = ${my.port}", "my.port = 2003", TIMEOUT + " = 20");

    BBjAdminConfiguration first = BBjAdminConfiguration.create(config);
    BBjAdminConfiguration second = BBjAdminConfiguration.create(config);

    assertEquals(first.toString(), second.toString());
    assertEquals(first.getPort(), second.getPort());
    assertFalse(config.isResolved());
  }
}
