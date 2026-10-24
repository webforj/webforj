package com.webforj.bbj.security.fixture;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

/**
 * Collects the records of one logger and the loggers below it for the length of a test.
 */
public final class CapturedLog implements AutoCloseable {
  private final Logger logger;
  private final Level previousLevel;
  private final List<LogRecord> records = new ArrayList<>();
  private final Object lock = new Object();
  private final Formatter formatter = new SimpleFormatter();
  private final Handler handler = new Handler() {
    @Override
    public void publish(LogRecord entry) {
      synchronized (lock) {
        records.add(entry);
        lock.notifyAll();
      }
    }

    @Override
    public void flush() {}

    @Override
    public void close() {}
  };

  /**
   * Starts collecting every level of the named logger.
   *
   * @param loggerName the name of the logger
   */
  public CapturedLog(String loggerName) {
    this.logger = Logger.getLogger(loggerName);
    this.previousLevel = logger.getLevel();
    logger.setLevel(Level.ALL);
    logger.addHandler(handler);
  }

  /**
   * Gets the formatted messages of one level.
   *
   * @param level the level of the records
   * @return the messages in the order they were logged
   */
  public List<String> getMessages(Level level) {
    synchronized (lock) {
      return records.stream().filter(entry -> entry.getLevel() == level)
          .map(formatter::formatMessage).toList();
    }
  }

  /**
   * Waits until a record of one level with the given message was logged.
   *
   * @param level the level of the record
   * @param message the formatted message of the record
   * @param timeout the longest time to wait
   * @param unit the unit of the timeout
   * @return {@code true} if the record was logged in time
   * @throws InterruptedException if the wait is interrupted
   */
  public boolean awaitMessage(Level level, String message, long timeout, TimeUnit unit)
      throws InterruptedException {
    long deadline = System.nanoTime() + unit.toNanos(timeout);
    synchronized (lock) {
      while (!records.stream().anyMatch(
          entry -> entry.getLevel() == level && message.equals(formatter.formatMessage(entry)))) {
        long remaining = deadline - System.nanoTime();
        if (remaining <= 0) {
          return false;
        }
        TimeUnit.NANOSECONDS.timedWait(lock, remaining);
      }

      return true;
    }
  }

  /**
   * Gets the formatted messages of every level, with the text of any attached exception.
   *
   * @return the messages in the order they were logged
   */
  public List<String> getAllMessages() {
    synchronized (lock) {
      return records.stream().map(entry -> formatter.formatMessage(entry)
          + (entry.getThrown() == null ? "" : " " + entry.getThrown())).toList();
    }
  }

  /**
   * Stops collecting.
   */
  @Override
  public void close() {
    logger.removeHandler(handler);
    logger.setLevel(previousLevel);
  }
}
