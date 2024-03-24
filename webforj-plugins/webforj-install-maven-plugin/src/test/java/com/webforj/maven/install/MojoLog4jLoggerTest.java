package com.webforj.maven.install;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertTrue;

import nl.altindag.log.LogCaptor;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class MojoLog4jLoggerTest {
  private static LogCaptor logCaptor;
  private MojoLog4jLogger mojoLog4jLogger;

  @BeforeAll
  public static void setupLogCaptor() {
    logCaptor = LogCaptor.forClass(MojoLog4jLogger.class);
  }

  @BeforeEach
  void setUp() {
    mojoLog4jLogger = new MojoLog4jLogger();
  }

  @AfterEach
  public void clearLogs() {
    logCaptor.clearLogs();
  }

  @AfterAll
  public static void tearDown() {
    logCaptor.close();
  }

  @Test
  void isDebugEnabled() {
    logCaptor.setLogLevelToDebug();
    assertTrue(mojoLog4jLogger.isDebugEnabled());
  }

  @Test
  void debug() {
    logCaptor.setLogLevelToDebug();
    mojoLog4jLogger.debug("DEBUG MESSAGE");
    assertTrue(logCaptor.getLogs().contains("DEBUG MESSAGE"));

  }

  @Test
  void debugWithException() {
    assertDoesNotThrow(() -> mojoLog4jLogger.debug(new Exception()));
  }

  @Test
  void debugWithMessageAndException() {
    assertDoesNotThrow(() -> mojoLog4jLogger.debug("message", new Exception()));
  }

  @Test
  void info() {
    assertDoesNotThrow(() -> mojoLog4jLogger.info(""));
  }

  @Test
  void isInfoEnabled() {
    assertTrue(mojoLog4jLogger.isInfoEnabled());
  }

  @Test
  void infogWithException() {
    assertDoesNotThrow(() -> mojoLog4jLogger.info(new Exception()));
  }

  @Test
  void infoWithMessageAndException() {
    assertDoesNotThrow(() -> mojoLog4jLogger.info("message", new Exception()));
  }

  @Test
  void warn() {
    assertDoesNotThrow(() -> mojoLog4jLogger.warn(""));
  }

  @Test
  void isWarnEnabled() {
    assertTrue(mojoLog4jLogger.isWarnEnabled());
  }

  @Test
  void warnWithException() {
    assertDoesNotThrow(() -> mojoLog4jLogger.warn(new Exception()));
  }

  @Test
  void warnWithMessageAndException() {
    assertDoesNotThrow(() -> mojoLog4jLogger.warn("message", new Exception("exception message")));
  }



  @Test
  void isErrorEnabled() {
    assertTrue(mojoLog4jLogger.isErrorEnabled());

  }

  @Test
  void error() {
    assertDoesNotThrow(() -> mojoLog4jLogger.error(""));
  }


  @Test
  void errorWithException() {
    assertDoesNotThrow(() -> mojoLog4jLogger.error(new Exception()));
  }

  @Test
  void errorWithMessageAndException() {
    assertDoesNotThrow(() -> mojoLog4jLogger.error("message", new Exception()));
  }


}
