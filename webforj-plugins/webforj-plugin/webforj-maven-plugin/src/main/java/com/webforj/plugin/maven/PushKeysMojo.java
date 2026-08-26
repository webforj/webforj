package com.webforj.plugin.maven;

import com.webforj.plugin.foundation.push.PushKeyCommand;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * Generates a push key pair and prints the configuration lines the application reads.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@Mojo(name = "push-keys", requiresProject = false, threadSafe = true)
public class PushKeysMojo extends AbstractMojo {

  /**
   * {@inheritDoc}
   */
  @Override
  public void execute() {
    for (String line : PushKeyCommand.render()) {
      getLog().info(line);
    }
  }
}
