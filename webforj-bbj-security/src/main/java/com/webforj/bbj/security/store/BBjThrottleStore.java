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
 * Time is split into periods of the window length, counted from the epoch. A failed attempt counts
 * in the period it happened in. The failed attempts of a user name or an address are those of the
 * current period plus those of the previous period, weighted by the share of the previous period
 * that is still inside the window that ends now. A period counts as 0 once two windows have passed
 * since its start. The file holds a hash of the user name or the address, never the text itself.
 * The counters of one application are shared by every browser and every page load on the server.
 * </p>
 *
 * <p>
 * Every write removes the counters of the periods that no longer count and, at the limit of 10000
 * counters, the counters that expire first. A failure is logged by its class name only.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjThrottleStore {
  private static final Logger logger = System.getLogger(BBjThrottleStore.class.getName());
  private static final String VERSION = "t2.";
  private static final int HASH_BYTES = 16;
  private static final String NOT_USABLE = "The sign in counters could not be used";
  private static final String FAILURE = "{0} ({1})";
  private final BBjEntryFile file;
  private final long periodMillis;
  private final Clock clock;

  BBjThrottleStore(BBjEntryFile file, Duration window, Clock clock) {
    this.file = file;
    this.periodMillis = window.toMillis();
    this.clock = clock;
  }

  /**
   * Creates the throttle store of the current application.
   *
   * @param window the time the failed attempts are counted in
   * @return the throttle store
   */
  public static BBjThrottleStore create(Duration window) {
    var file = new BBjEntryFile(() -> Path.of(System.getProperty("java.io.tmpdir")),
        App::getApplicationName, Content.COUNTERS);
    return new BBjThrottleStore(file, window, Clock.systemUTC());
  }

  /**
   * Reads the failed attempts of a user name or an address inside the window that ends now.
   *
   * @param kind whether the value is a user name or an address
   * @param value the user name or the address
   * @return the failed attempts, 0 when there is no counter inside the window
   *
   * @throws IllegalStateException if the counters cannot be read
   */
  public int readAttempts(Kind kind, String value) {
    String key = createKey(kind, value);
    try {
      long now = clock.instant().toEpochMilli();
      long periodStart = toPeriodStart(now);
      Map<String, String> entries = file.readEntries();
      return findCount(entries, key, periodStart)
          + findPreviousWeight(entries, key, periodStart, now);
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
   * @return the failed attempts inside the window that ends now, this one included
   *
   * @throws IllegalStateException if the counters cannot be written
   */
  public int addFailure(Kind kind, String value) {
    String key = createKey(kind, value);
    int[] result = {0};
    try {
      Instant now = clock.instant();
      long periodStart = toPeriodStart(now.toEpochMilli());
      file.updateEntries(entries -> {
        removeStaleEntries(entries, now);
        int current = findCount(entries, key, periodStart) + 1;
        entries.put(createName(key, periodStart), String.valueOf(current));
        result[0] = current + findPreviousWeight(entries, key, periodStart, now.toEpochMilli());
      });
    } catch (IOException | RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "A failed sign in could not be counted",
          e.getClass().getName());
      throw new IllegalStateException(NOT_USABLE);
    }

    return result[0];
  }

  /**
   * Removes the counters of a user name or an address.
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
      long periodStart = toPeriodStart(now.toEpochMilli());
      file.updateEntries(entries -> {
        removeStaleEntries(entries, now);
        entries.remove(createName(key, periodStart));
        entries.remove(createName(key, periodStart - periodMillis));
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
          "The throttle store removed {0} counters because its limit of {1} was reached",
          String.valueOf(removed), String.valueOf(file.getMaxEntries()));
    }
  }

  private int findCount(Map<String, String> entries, String key, long periodStart) {
    return Optional.ofNullable(entries.get(createName(key, periodStart))).map(Integer::parseInt)
        .orElse(0);
  }

  private int findPreviousWeight(Map<String, String> entries, String key, long periodStart,
      long now) {
    long previous = findCount(entries, key, periodStart - periodMillis);
    return (int) Math.ceilDiv(previous * (periodMillis - (now - periodStart)), periodMillis);
  }

  private long toPeriodStart(long now) {
    return Math.floorDiv(now, periodMillis) * periodMillis;
  }

  private String createName(String key, long periodStart) {
    return VERSION + (periodStart + 2 * periodMillis) + "." + periodStart + "." + key;
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
}
