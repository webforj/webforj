package com.webforj.plugin.foundation.hotswap.jrebel;

import com.webforj.plugin.foundation.hotswap.HotswapAttachment;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Locale;
import java.util.function.Consumer;

/**
 * Attaches the JRebel agent to the application virtual machine.
 *
 * <p>
 * JRebel ships as a native library on every platform and additionally as a jar, and the two forms
 * need different flags. The flag is picked from the configured file, so the build configuration
 * only ever names a path.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class JrebelAttachment implements HotswapAttachment {

  static final String TOOL_ARGUMENT = "-Dwebforj.hotswap.tool=jrebel";
  static final String LEVEL_ARGUMENT = "-Dwebforj.hotswap.level=full";

  private final Path path;
  private final Consumer<String> log;

  private JrebelAttachment(Builder builder) {
    this.path = builder.path;
    this.log = builder.log;
  }

  /**
   * Creates a new builder for an attachment.
   *
   * @return a new builder
   */
  public static Builder create() {
    return new Builder();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> getArguments() throws IOException {
    if (!Files.isRegularFile(path)) {
      throw new IOException("the JRebel agent does not exist: " + path);
    }

    String name = path.getFileName().toString().toLowerCase(Locale.ROOT);
    String flag = name.endsWith(".jar") ? "-javaagent:" : "-agentpath:";
    log.accept("webforJ hotswap: JRebel attached to the application virtual machine from " + path);

    // The properties tell the application which tool this attachment installed. JRebel carries
    // its own redefinition support on every virtual machine.
    return List.of(flag + path.toAbsolutePath(), TOOL_ARGUMENT, LEVEL_ARGUMENT);
  }

  /**
   * Builds a {@link JrebelAttachment}.
   *
   * @author Hyyan Abo Fakher
   * @since 26.02
   */
  public static final class Builder {

    private Path path;
    private Consumer<String> log = line -> {
    };

    private Builder() {}

    /**
     * Sets the JRebel agent on disk, a native library or a jar.
     *
     * @param path the agent path
     * @return this builder
     */
    public Builder setPath(Path path) {
      this.path = path;
      return this;
    }

    /**
     * Sets where progress lines are reported.
     *
     * @param log the log sink
     * @return this builder
     */
    public Builder setLog(Consumer<String> log) {
      this.log = log != null ? log : line -> {
      };
      return this;
    }

    /**
     * Builds the attachment.
     *
     * @return the attachment
     */
    public JrebelAttachment build() {
      return new JrebelAttachment(this);
    }
  }
}
