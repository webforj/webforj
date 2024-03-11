package com.webforj.installer;

import java.io.IOException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.maven.shared.invoker.InvocationOutputHandler;

/**
 * This class redirects the output of the maven-invoker to a StringBuilder instance´ that is
 * maintained by its creator.
 */
public class MavenOutputHandler implements InvocationOutputHandler {
  Logger log = LogManager.getLogger("webforj-installer");

  private final StringBuilder out;

  MavenOutputHandler(StringBuilder sb) {
    this.out = sb;
  }

  /**
   * Consumes a line of log.
   *
   * @param line is the line of log received by the invoker.
   * @throws IOException when there is a problem with consuming the line.
   */
  @Override
  public void consumeLine(String line) throws IOException {
    log.info(line);
    out.append("webforj-installer: " + line + "\n");
  }
}
