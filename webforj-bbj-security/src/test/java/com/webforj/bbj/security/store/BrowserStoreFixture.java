package com.webforj.bbj.security.store;

import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.webstorage.LocalStorage;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import java.util.logging.SimpleFormatter;

/** Holds the values and the browser stores the tests of the browser store share. */
public final class BrowserStoreFixture {
  /** The name of the logger of the browser store. */
  public static final String LOGGER_NAME = BBjBrowserSecurityStore.class.getName();
  /** The browser storage name of the entry name of the application {@link #APP}. */
  public static final String HANDLE_KEY = "webforj-bbj-handle.shop";
  /** The browser storage name of the key of the application {@link #APP}. */
  public static final String KEY_KEY = "webforj-bbj-key.shop";
  /** The application name the tests use. */
  public static final String APP = "shop";
  /** The token of the session the tests save. */
  public static final String TOKEN = "token-secret-123";
  /** The time the fixed clock of the tests shows. */
  public static final Instant NOW = Instant.parse("2026-01-01T00:00:00Z");
  /** The expiry of the session the tests save. */
  public static final Instant EXPIRES_AT = NOW.plusSeconds(3600);

  private BrowserStoreFixture() {}

  /**
   * Creates a browser store with the system clock.
   *
   * @param directorySupplier gives the directory of the session file
   * @param applicationNameSupplier gives the application name
   * @param storageSupplier gives the local storage of the browser
   *
   * @return the browser store
   */
  public static BBjSecurityStore createBrowserStore(Supplier<Path> directorySupplier,
      Supplier<String> applicationNameSupplier, Supplier<LocalStorage> storageSupplier) {
    return new BBjBrowserSecurityStore(directorySupplier, applicationNameSupplier, storageSupplier);
  }

  /**
   * Creates a browser store with a clock.
   *
   * @param directorySupplier gives the directory of the session file
   * @param applicationNameSupplier gives the application name
   * @param storageSupplier gives the local storage of the browser
   * @param clock the clock of the store
   *
   * @return the browser store
   */
  public static BBjSecurityStore createBrowserStore(Supplier<Path> directorySupplier,
      Supplier<String> applicationNameSupplier, Supplier<LocalStorage> storageSupplier,
      Clock clock) {
    return new BBjBrowserSecurityStore(directorySupplier, applicationNameSupplier, storageSupplier,
        clock);
  }

  /**
   * Creates the part of the file name and of the browser storage names that names an application.
   *
   * @param applicationName the application name
   *
   * @return the part that names the application
   */
  public static String createApplicationKey(String applicationName) {
    return BBjBrowserSecurityStore.createApplicationKey(applicationName);
  }

  /**
   * Creates a mocked local storage that keeps its values in a map.
   *
   * @param values the values of the local storage
   *
   * @return the local storage
   */
  public static LocalStorage createStorage(Map<String, String> values) {
    LocalStorage target = mock(LocalStorage.class);
    when(target.get(anyList())).thenAnswer(call -> {
      Map<String, String> pair = new HashMap<>();
      call.<List<String>>getArgument(0).stream().filter(values::containsKey)
          .forEach(key -> pair.put(key, values.get(key)));
      return pair;
    });
    when(target.add(anyMap())).thenAnswer(call -> {
      values.putAll(call.<Map<String, String>>getArgument(0));
      return target;
    });
    doAnswer(call -> {
      values.remove(HANDLE_KEY);
      values.remove(KEY_KEY);
      return null;
    }).when(target).remove(HANDLE_KEY, KEY_KEY);

    return target;
  }

  /**
   * Creates the name of an entry of the session file.
   *
   * @param expiresAt the expiry the name carries
   * @param index the number that makes the name unique
   *
   * @return the name of the entry
   */
  public static String createName(Instant expiresAt, int index) {
    return "v1." + expiresAt.toEpochMilli() + "." + String.format("%043d", index);
  }

  /**
   * Creates a log handler that collects the formatted messages.
   *
   * @param messages the list the messages are added to
   *
   * @return the log handler
   */
  public static Handler createHandler(List<String> messages) {
    return new Handler() {
      @Override
      public void publish(LogRecord logRecord) {
        messages.add(new SimpleFormatter().formatMessage(logRecord));
      }

      @Override
      public void flush() {
        // Not implemented
      }

      @Override
      public void close() {
        // Not implemented
      }
    };
  }
}
