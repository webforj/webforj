package com.webforj.bbj.security.store;

import com.webforj.App;
import com.webforj.bbj.security.store.BBjEntryFile.Content;
import java.io.IOException;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.nio.file.Path;
import java.text.Normalizer;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;

/**
 * Counts the failed sign in attempts per user name and per client address in a file on the server.
 *
 * <p>
 * A counter starts at the first failed attempt and counts for one window. A counter whose window
 * has ended counts as 0, and the next failed attempt starts a new window. The file holds a hash of
 * the user name or the address, never the text itself. The counters of one application are shared
 * by every browser and every page load on the server.
 * </p>
 *
 * <p>
 * Every write removes the counters whose window has ended and, at the limit of 10000 counters, the
 * counters whose window ends first. A failure is logged by its class name only.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjThrottleStore {
  private static final Logger logger = System.getLogger(BBjThrottleStore.class.getName());
  private static final String VERSION = "t1.";
  private static final int HASH_BYTES = 16;
  private static final String NOT_USABLE = "The sign in counters could not be used";
  private static final String FAILURE = "{0} ({1})";
  private final BBjEntryFile file;
  private final Duration window;
  private final Clock clock;

  BBjThrottleStore(BBjEntryFile file, Duration window, Clock clock) {
    this.file = file;
    this.window = window;
    this.clock = clock;
  }

  /**
   * Creates the throttle store of the current application.
   *
   * @param window the time a counter counts from its first failed attempt
   * @return the throttle store
   */
  public static BBjThrottleStore create(Duration window) {
    var file = new BBjEntryFile(() -> Path.of(System.getProperty("java.io.tmpdir")),
        App::getApplicationName, Content.COUNTERS);
    return new BBjThrottleStore(file, window, Clock.systemUTC());
  }

  /**
   * Reads the failed attempts of a user name or an address inside the current window.
   *
   * @param kind whether the value is a user name or an address
   * @param value the user name or the address
   * @return the failed attempts, 0 when there is no counter or its window has ended
   *
   * @throws IllegalStateException if the counters cannot be read
   */
  public int readAttempts(Kind kind, String value) {
    String key = createKey(kind, value);
    try {
      Instant now = clock.instant();
      return findCounter(file.readEntries(), key, now).map(BBjThrottleCounter::getCounter)
          .orElse(0);
    } catch (IOException | RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The sign in counters could not be read",
          e.getClass().getName());
      throw new IllegalStateException(NOT_USABLE);
    }
  }

  /**
   * Adds a failed attempt to the counter of a user name or an address.
   *
   * @param kind whether the value is a user name or an address
   * @param value the user name or the address
   * @return the failed attempts inside the current window, this one included
   *
   * @throws IllegalStateException if the counters cannot be written
   */
  public int addFailure(Kind kind, String value) {
    String key = createKey(kind, value);
    int[] result = {0};
    try {
      Instant now = clock.instant();
      file.updateEntries(entries -> {
        removeStaleEntries(entries, now);
        Optional<String> name = findName(entries, key);
        Optional<BBjThrottleCounter> counter = findCounter(entries, key, now);
        if (counter.isPresent()) {
          result[0] = counter.get().getCounter() + 1;
          entries.put(name.orElseThrow(),
              result[0] + "." + counter.get().getWindowStart().toEpochMilli());
        } else {
          name.ifPresent(entries::remove);
          result[0] = 1;
          entries.put(VERSION + now.plus(window).toEpochMilli() + "." + key,
              "1." + now.toEpochMilli());
        }
      });
    } catch (IOException | RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "A failed sign in could not be counted",
          e.getClass().getName());
      throw new IllegalStateException(NOT_USABLE);
    }

    return result[0];
  }

  /**
   * Removes the counter of a user name or an address.
   *
   * @param kind whether the value is a user name or an address
   * @param value the user name or the address
   *
   * @throws IllegalStateException if the counters cannot be written
   */
  public void clearAttempts(Kind kind, String value) {
    String key = createKey(kind, value);
    try {
      Instant now = clock.instant();
      file.updateEntries(entries -> {
        removeStaleEntries(entries, now);
        findName(entries, key).ifPresent(entries::remove);
      });
    } catch (IOException | RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The failed sign ins could not be cleared",
          e.getClass().getName());
      throw new IllegalStateException(NOT_USABLE);
    }
  }

  private void removeStaleEntries(Map<String, String> entries, Instant now) {
    int removed = file.removeStaleEntries(entries, now);
    if (removed > 0) {
      logger.log(Level.WARNING,
          "The throttle store removed {0} counters because its limit of {1} was reached", removed,
          String.valueOf(file.getMaxEntries()));
      // must be wrapped in String.valueOf or else the String gets localized
    }
  }

  private Optional<String> findName(Map<String, String> entries, String key) {
    return entries.keySet().stream().filter(name -> name.endsWith("." + key)).findFirst();
  }

  private Optional<BBjThrottleCounter> findCounter(Map<String, String> entries, String key,
      Instant now) {
    return findName(entries, key).map(entries::get).map(BBjThrottleStore::parseCounter)
        .filter(counter -> now.isBefore(counter.getWindowStart().plus(window)));
  }

  private static BBjThrottleCounter parseCounter(String value) {
    int split = value.indexOf('.');
    return new BBjThrottleCounter(Integer.parseInt(value.substring(0, split)),
        Instant.ofEpochMilli(Long.parseLong(value.substring(split + 1))));
  }

  private static String createKey(Kind kind, String value) {
    return kind.getPrefix()
        + BBjEntryFile.createHash(kind.name() + "\0" + toNormalText(kind, value), HASH_BYTES);
  }

  private static String toNormalText(Kind kind, String value) {
    String normal = Normalizer.normalize(value, Normalizer.Form.NFC).strip();
    return kind == Kind.ADDRESS ? normal.toLowerCase(Locale.ROOT) : normal;
  }

  /**
   * Whether a counter belongs to a user name or to a client address.
   *
   * @author Christian Liebhardt
   * @since 26.03
   */
  public enum Kind {
    /** The counter of a user name. */
    USER_NAME("u."),

    /** The counter of a client address. */
    ADDRESS("a.");

    private final String prefix;

    Kind(String prefix) {
      this.prefix = prefix;
    }

    String getPrefix() {
      return prefix;
    }
  }

  /**
   * The failed attempts of one counter and the start of its window.
   *
   * @author Christian Liebhardt
   * @since 26.03
   */
  private static final class BBjThrottleCounter {
    private final int counter;
    private final Instant windowStart;

    BBjThrottleCounter(int counter, Instant windowStart) {
      this.counter = counter;
      this.windowStart = windowStart;
    }

    int getCounter() {
      return counter;
    }

    Instant getWindowStart() {
      return windowStart;
    }
  }
}
