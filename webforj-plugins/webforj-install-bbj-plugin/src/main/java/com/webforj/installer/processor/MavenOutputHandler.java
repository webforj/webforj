package com.webforj.installer.processor;

import java.io.PrintStream;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.maven.shared.invoker.PrintStreamHandler;

/**
 * This class redirects the output of the maven-invoker to a Printstream instance that is maintained
 * by its creator.
 */
public class MavenOutputHandler extends PrintStreamHandler {
  Logger log = LogManager.getLogger("webforj-installer");

  /**
   * Constructor requires same arguments as {@link PrintStreamHandler} ancestor.
   *
   * @param printStream the output stream.
   * @param autoflush whether to flush after each write or not.
   */
  MavenOutputHandler(PrintStream printStream, boolean autoflush) {
    super(printStream, autoflush);
  }

  /**
   * {@inheritDoc}
   * <p/>
   * Override so we can add the line to the log as well. Remember that we are using
   * PrintStreamHandler, which calls on a println method to output.
   *
   * @param line the line to print.
   */
  @Override
  public void consumeLine(String line) {
    log.info(line);
    super.consumeLine("webforj-installer: " + line);
  }
}
