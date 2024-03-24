package com.webforj.maven.install;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.maven.plugin.logging.Log;

public class MojoLog4jLogger implements Log {

  static final Logger logger = LogManager.getLogger(MojoLog4jLogger.class);


  /**
   * @return true if the <b>debug</b> error level is enabled
   */
  @Override
  public boolean isDebugEnabled() {
    return logger.isDebugEnabled();
  }

  /**
   * Send a message to the user in the <b>debug</b> error level.
   *
   * @param content content message.
   */
  @Override
  public void debug(CharSequence content) {
    logger.debug(content);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void debug(CharSequence content, Throwable error) {
    logger.debug(content, error);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void debug(Throwable error) {
    logger.debug(error);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isInfoEnabled() {
    return logger.isInfoEnabled();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void info(CharSequence content) {
    logger.info(content);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void info(CharSequence content, Throwable error) {
    logger.info(content, error);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void info(Throwable error) {
    logger.info(error);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isWarnEnabled() {
    return logger.isWarnEnabled();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void warn(CharSequence content) {
    logger.warn(content);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void warn(CharSequence content, Throwable error) {
    logger.warn(content, error);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void warn(Throwable error) {
    logger.warn(error);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isErrorEnabled() {
    return logger.isErrorEnabled();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void error(CharSequence content) {
    logger.error(content);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void error(CharSequence content, Throwable error) {
    logger.error(content, error);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void error(Throwable error) {
    logger.error(error);
  }
}
