package com.webforj.plugin.foundation.bbj;

import java.io.IOException;
import java.time.Duration;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.function.LongSupplier;

/**
 * Checks a condition at an interval until it holds or a timeout passes, and reports the wait at a
 * progress interval.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class PollingWait {

  private static final Duration PROGRESS_INTERVAL = Duration.ofSeconds(10);

  private final String subject;
  private final Duration timeout;
  private final Duration interval;
  private final Duration progressInterval;
  private final String progressMessage;
  private final Consumer<String> log;
  private final LongSupplier clock;
  private final Sleeper sleeper;

  private PollingWait(Builder builder) {
    this.subject = builder.subject;
    this.timeout = builder.timeout;
    this.interval = builder.interval;
    this.progressInterval = builder.progressInterval;
    this.progressMessage = builder.progressMessage;
    this.log = builder.log;
    this.clock = builder.clock;
    this.sleeper = builder.sleeper;
  }

  /**
   * Creates a builder for a wait.
   *
   * @param subject what is waited for, named in the failure of an interrupted wait
   * @param timeout the longest wait
   * @param interval the pause between two checks
   *
   * @return the builder
   * @throws NullPointerException if an argument is null
   */
  public static Builder create(String subject, Duration timeout, Duration interval) {
    return new Builder(subject, timeout, interval);
  }

  /**
   * Checks the condition until it holds or the timeout passes, with the interval between two
   * checks.
   *
   * <p>
   * While the condition does not hold, the progress message followed by the seconds waited is
   * reported at the progress interval.
   * </p>
   *
   * @param <E> the failure of a check
   * @param condition the condition
   *
   * @return the time waited until the condition held, empty when the timeout passed first
   * @throws E if a check fails
   * @throws IOException if the thread is interrupted, which keeps the interrupt status of the
   *         thread
   */
  public <E extends Exception> Optional<Duration> apply(Condition<E> condition)
      throws E, IOException {
    long start = clock.getAsLong();
    long report = progressInterval.toNanos();
    try {
      while (!condition.isMet()) {
        if (clock.getAsLong() - start - timeout.toNanos() >= 0) {
          return Optional.empty();
        }

        sleeper.sleep(interval);
        long elapsed = clock.getAsLong() - start;
        if (progressMessage != null && elapsed >= report) {
          log.accept(progressMessage + TimeUnit.NANOSECONDS.toSeconds(elapsed) + " seconds");
          report = elapsed - elapsed % progressInterval.toNanos() + progressInterval.toNanos();
        }
      }
    } catch (InterruptedException interruption) {
      Thread.currentThread().interrupt();
      throw new IOException(subject + " was interrupted", interruption);
    }

    return Optional.of(Duration.ofNanos(clock.getAsLong() - start));
  }

  /**
   * Checks whether a wait is over.
   *
   * @param <E> the failure of a check
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  @FunctionalInterface
  public interface Condition<E extends Exception> {

    /**
     * Checks whether the condition holds.
     *
     * @return true when the condition holds
     * @throws E if the check fails, which ends the wait
     * @throws InterruptedException if the thread is interrupted during the check
     */
    boolean isMet() throws E, InterruptedException;
  }

  /**
   * Pauses the thread between two checks of a wait.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  @FunctionalInterface
  public interface Sleeper {

    /**
     * Pauses the thread.
     *
     * @param duration how long to pause
     *
     * @throws InterruptedException if the thread is interrupted during the pause
     */
    void sleep(Duration duration) throws InterruptedException;
  }

  /**
   * Builds a {@link PollingWait}.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  public static final class Builder {

    private final String subject;
    private final Duration timeout;
    private final Duration interval;
    private Duration progressInterval = PROGRESS_INTERVAL;
    private String progressMessage;
    private Consumer<String> log = line -> {
    };
    private LongSupplier clock = System::nanoTime;
    private Sleeper sleeper = Thread::sleep;

    private Builder(String subject, Duration timeout, Duration interval) {
      this.subject = Objects.requireNonNull(subject);
      this.timeout = Objects.requireNonNull(timeout);
      this.interval = Objects.requireNonNull(interval);
    }

    /**
     * Sets where the progress of the wait is reported.
     *
     * @param log the log sink
     *
     * @return this builder
     * @throws NullPointerException if log is null
     */
    public Builder setLog(Consumer<String> log) {
      this.log = Objects.requireNonNull(log);
      return this;
    }

    /**
     * Sets the text the seconds waited follow in a progress report. Without it the wait reports
     * nothing.
     *
     * @param progressMessage the text of a progress report
     *
     * @return this builder
     * @throws NullPointerException if progressMessage is null
     */
    public Builder setProgressMessage(String progressMessage) {
      this.progressMessage = Objects.requireNonNull(progressMessage);
      return this;
    }

    /**
     * Sets how often the wait is reported.
     *
     * @param progressInterval the progress interval, 10 seconds by default
     *
     * @return this builder
     * @throws NullPointerException if progressInterval is null
     */
    public Builder setProgressInterval(Duration progressInterval) {
      this.progressInterval = Objects.requireNonNull(progressInterval);
      return this;
    }

    /**
     * Sets the clock the wait reads its elapsed time from.
     *
     * @param clock the clock in nanoseconds, {@link System#nanoTime()} by default
     *
     * @return this builder
     * @throws NullPointerException if clock is null
     */
    public Builder setClock(LongSupplier clock) {
      this.clock = Objects.requireNonNull(clock);
      return this;
    }

    /**
     * Sets how the wait pauses between two checks.
     *
     * @param sleeper the pause, {@link Thread#sleep(Duration)} by default
     *
     * @return this builder
     * @throws NullPointerException if sleeper is null
     */
    public Builder setSleeper(Sleeper sleeper) {
      this.sleeper = Objects.requireNonNull(sleeper);
      return this;
    }

    /**
     * Builds the wait.
     *
     * @return the wait
     */
    public PollingWait build() {
      return new PollingWait(this);
    }
  }
}
