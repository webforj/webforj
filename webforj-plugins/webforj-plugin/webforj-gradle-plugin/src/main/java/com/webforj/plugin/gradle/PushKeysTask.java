package com.webforj.plugin.gradle;

import com.webforj.plugin.foundation.push.PushKeyCommand;
import org.gradle.api.DefaultTask;
import org.gradle.api.tasks.TaskAction;
import org.gradle.work.DisableCachingByDefault;

/**
 * Generates a push key pair and prints the configuration lines the application reads.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@DisableCachingByDefault(because = "every run must generate a fresh key pair")
public abstract class PushKeysTask extends DefaultTask {

  /**
   * Generates the keys and prints them.
   */
  @TaskAction
  public void generate() {
    for (String line : PushKeyCommand.render()) {
      getLogger().lifecycle(line);
    }
  }
}
