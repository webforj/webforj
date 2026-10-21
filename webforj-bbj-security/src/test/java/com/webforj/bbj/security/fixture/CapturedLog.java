package com.webforj.bbj.security.fixture;

import java.util.ArrayList;
import java.util.List;
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
  private final Formatter formatter = new SimpleFormatter();
  private final Handler handler = new Handler() {
    @Override
    public void publish(LogRecord entry) {
      records.add(entry);
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
    return records.stream().filter(entry -> entry.getLevel() == level).map(formatter::formatMessage)
        .toList();
  }

  /**
   * Gets the formatted messages of every level, with the text of any attached exception.
   *
   * @return the messages in the order they were logged
   */
  public List<String> getAllMessages() {
    return records.stream().map(entry -> formatter.formatMessage(entry)
        + (entry.getThrown() == null ? "" : " " + entry.getThrown())).toList();
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
