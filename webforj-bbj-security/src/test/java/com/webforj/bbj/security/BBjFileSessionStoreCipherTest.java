package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bbj.security.BBjFileSessionStore.BBjSessionEntryCipher;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.bbj.security.model.BBjSession;
import java.time.Instant;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjFileSessionStoreCipherTest {
  private static final Pattern URL_SAFE_43 = Pattern.compile("[A-Za-z0-9_-]{43}");
  private static final String ENTRY_NAME = "v1.1767225600000." + "h".repeat(43);
  private static final String TOKEN = "token-secret-123";
  private static final Instant EXPIRES_AT = Instant.parse("2026-01-01T01:00:00Z");

  private BBjSessionEntryCipher cipher;
  private BBjSession session;

  @BeforeEach
  void setUp() {
    cipher = new BBjSessionEntryCipher();
    session = new BBjSession(new BBjPrincipal("alice", Set.of("ADMIN", "USER"), Set.of("read")),
        TOKEN, EXPIRES_AT);
  }

  @Test
  void shouldCreateRandomUrlSafeHandleAndKey() {
    assertTrue(URL_SAFE_43.matcher(cipher.createHandle()).matches());
    assertTrue(URL_SAFE_43.matcher(cipher.createKey()).matches());
    assertNotEquals(cipher.createKey(), cipher.createKey());
  }

  @Test
  void shouldReadBackTheSessionWithRolesAndAuthorities() {
    String key = cipher.createKey();
    String entry = cipher.formatEntry(ENTRY_NAME, key, session);

    BBjSession read = cipher.parseEntry(ENTRY_NAME, key, entry).orElseThrow();

    assertEquals("alice", read.getPrincipal().getUserName());
    assertEquals(Set.of("ADMIN", "USER"), read.getPrincipal().getRoles());
    assertEquals(Set.of("read"), read.getPrincipal().getAuthorities());
    assertEquals(TOKEN, read.getToken());
    assertEquals(EXPIRES_AT, read.getExpiresAt());
  }

  @Test
  void shouldReadBackSessionWithoutRolesAndAuthorities() {
    BBjSession bare =
        new BBjSession(new BBjPrincipal("bob", Set.of(), Set.of()), TOKEN, EXPIRES_AT);
    String key = cipher.createKey();

    BBjSession read =
        cipher.parseEntry(ENTRY_NAME, key, cipher.formatEntry(ENTRY_NAME, key, bare)).orElseThrow();

    assertTrue(read.getPrincipal().getRoles().isEmpty());
    assertTrue(read.getPrincipal().getAuthorities().isEmpty());
  }

  @Test
  void shouldNotShowTheTokenInTheEntry() {
    String entry = cipher.formatEntry(ENTRY_NAME, cipher.createKey(), session);

    assertFalse(entry.contains(TOKEN));
    assertTrue(Pattern.matches("[A-Za-z0-9_-]+", entry));
  }

  @Test
  void shouldMakeAnotherEntryOnEveryCall() {
    String key = cipher.createKey();

    assertNotEquals(cipher.formatEntry(ENTRY_NAME, key, session),
        cipher.formatEntry(ENTRY_NAME, key, session));
  }

  @ParameterizedTest
  @MethodSource("unreadableEntries")
  void shouldReadNothingWhenTheEntryCannotBeOpened(String entryName, String key, String entry) {
    assertEquals(Optional.empty(), cipher.parseEntry(entryName, key, entry));
  }

  @Test
  void shouldRefuseKeyThatIsNotBase64() {
    String key = "not a key!";

    IllegalStateException thrown = assertThrows(IllegalStateException.class,
        () -> cipher.formatEntry(ENTRY_NAME, key, session));

    assertEquals("The sign in could not be encrypted", thrown.getMessage());
    assertNull(thrown.getCause());
  }

  @Test
  void shouldRefuseSessionThatIsTooLargeToEncrypt() {
    BBjSession large = new BBjSession(session.getPrincipal(), "t".repeat(70000), EXPIRES_AT);
    String key = cipher.createKey();

    IllegalStateException thrown =
        assertThrows(IllegalStateException.class, () -> cipher.formatEntry(ENTRY_NAME, key, large));

    assertEquals("The sign in could not be encrypted", thrown.getMessage());
  }

  private static Stream<Arguments> unreadableEntries() {
    BBjSessionEntryCipher cipher = new BBjSessionEntryCipher();
    BBjSession session =
        new BBjSession(new BBjPrincipal("alice", Set.of(), Set.of()), TOKEN, EXPIRES_AT);
    String key = cipher.createKey();
    String entry = cipher.formatEntry(ENTRY_NAME, key, session);
    String changed = (entry.charAt(0) == 'A' ? 'B' : 'A') + entry.substring(1);

    return Stream.of(Arguments.of(ENTRY_NAME, cipher.createKey(), entry),
        Arguments.of("v1.1767225600001." + "h".repeat(43), key, entry),
        Arguments.of(ENTRY_NAME, key, changed), Arguments.of(ENTRY_NAME, key, "AAAA"),
        Arguments.of(ENTRY_NAME, key, "not base64!"), Arguments.of(ENTRY_NAME, "short", entry));
  }
}
