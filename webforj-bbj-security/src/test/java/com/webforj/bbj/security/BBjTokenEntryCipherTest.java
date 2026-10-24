package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Base64;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjTokenEntryCipherTest {
  private static final Pattern URL_SAFE_43 = Pattern.compile("[A-Za-z0-9_-]{43}");
  private static final String ENTRY_NAME = "v1.1792656616000." + "k".repeat(43);
  private static final Instant EXPIRES_AT = Instant.ofEpochMilli(1792656616000L);
  private final BBjTokenEntryCipher cipher = new BBjTokenEntryCipher();

  private static Stream<Arguments> sessions() {
    return Stream.of(Arguments.of("alice", "eyJhbGciOiJIUzI1NiJ9.payload.signature"),
        Arguments.of("Zoë Ünal 😀", "eyJ" + "x".repeat(3997)));
  }

  private static Stream<Arguments> unreadableEntries() {
    BBjTokenEntryCipher cipher = new BBjTokenEntryCipher();
    BBjLoginSession session = new BBjLoginSession("alice", "tokenValue", EXPIRES_AT);
    String key = cipher.createKey();
    String entry = cipher.formatEntry(ENTRY_NAME, key, session);
    char first = entry.charAt(20);
    String flipped = entry.substring(0, 20) + (first == 'A' ? 'B' : 'A') + entry.substring(21);

    return Stream.of(Arguments.of("wrong key", ENTRY_NAME, cipher.createKey(), entry),
        Arguments.of("other entry name", ENTRY_NAME.replace("1792656616000", "1792656616001"), key,
            entry),
        Arguments.of("flipped character", ENTRY_NAME, key, flipped),
        Arguments.of("truncated", ENTRY_NAME, key, entry.substring(0, entry.length() - 4)),
        Arguments.of("shorter than IV and tag", ENTRY_NAME, key, entry.substring(0, 20)),
        Arguments.of("empty", ENTRY_NAME, key, ""),
        Arguments.of("not Base64", ENTRY_NAME, key, "***not base64***"),
        Arguments.of("text of another version", ENTRY_NAME, key, "v2." + entry),
        Arguments.of("key that is not Base64", ENTRY_NAME, "***", entry),
        Arguments.of("key of the wrong length", ENTRY_NAME, "AAAA", entry));
  }

  @ParameterizedTest
  @MethodSource("sessions")
  void shouldReadBackTheUserNameTokenAndExpiry(String userName, String token) {
    String key = cipher.createKey();
    BBjLoginSession session = new BBjLoginSession(userName, token, EXPIRES_AT);

    Optional<BBjLoginSession> parsed =
        cipher.parseEntry(ENTRY_NAME, key, cipher.formatEntry(ENTRY_NAME, key, session));

    assertEquals(userName, parsed.orElseThrow().getUserName());
    assertEquals(token, parsed.orElseThrow().getToken());
    assertEquals(EXPIRES_AT, parsed.orElseThrow().getExpiresAt());
  }

  @Test
  void shouldGiveDifferentTextsForTheSameSession() {
    String key = cipher.createKey();
    BBjLoginSession session = new BBjLoginSession("alice", "tokenValue", EXPIRES_AT);

    assertNotEquals(cipher.formatEntry(ENTRY_NAME, key, session),
        cipher.formatEntry(ENTRY_NAME, key, session));
  }

  @Test
  void shouldHideTheUserNameAndTheTokenInTheText() {
    String key = cipher.createKey();
    BBjLoginSession session = new BBjLoginSession("alice", "tokenValue", EXPIRES_AT);

    String entry = cipher.formatEntry(ENTRY_NAME, key, session);
    String decoded = new String(Base64.getUrlDecoder().decode(entry), StandardCharsets.ISO_8859_1);

    assertFalse(entry.contains("alice") || entry.contains("tokenValue"));
    assertFalse(decoded.contains("alice") || decoded.contains("tokenValue"));
  }

  @ParameterizedTest(name = "{0}")
  @MethodSource("unreadableEntries")
  void shouldAnswerEmptyForAnEntryThatCannotBeRead(String reason, String entryName, String key,
      String entry) {
    Optional<BBjLoginSession> parsed =
        assertDoesNotThrow(() -> cipher.parseEntry(entryName, key, entry));

    assertEquals(Optional.empty(), parsed);
  }

  @Test
  void shouldCreateDifferentHandlesAndKeysOfFortyThreeUrlSafeCharacters() {
    String handle = cipher.createHandle();
    String key = cipher.createKey();

    assertTrue(URL_SAFE_43.matcher(handle).matches());
    assertTrue(URL_SAFE_43.matcher(key).matches());
    assertNotEquals(handle, cipher.createHandle());
    assertNotEquals(key, cipher.createKey());
  }

  @Test
  void shouldRefuseKeyThatIsNotAesKeyWithoutNamingIt() {
    BBjLoginSession session = new BBjLoginSession("alice", "tokenValue", EXPIRES_AT);

    IllegalStateException thrown = assertThrows(IllegalStateException.class,
        () -> cipher.formatEntry(ENTRY_NAME, "secretKeyText", session));

    assertFalse(thrown.getMessage().contains("secretKeyText"));
    assertNull(thrown.getCause());
  }

  @Test
  void shouldRefuseUserNameThatCannotBeWritten() {
    String key = cipher.createKey();
    BBjLoginSession session = new BBjLoginSession("a".repeat(70000), "tokenValue", EXPIRES_AT);

    IllegalStateException thrown = assertThrows(IllegalStateException.class,
        () -> cipher.formatEntry(ENTRY_NAME, key, session));

    assertNull(thrown.getCause());
  }
}
