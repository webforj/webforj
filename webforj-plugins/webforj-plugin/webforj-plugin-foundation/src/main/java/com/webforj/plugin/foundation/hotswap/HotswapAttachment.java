package com.webforj.plugin.foundation.hotswap;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

/**
 * A hotswap tool attached to the application virtual machine.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public interface HotswapAttachment {

  /**
   * Composes the virtual machine arguments that attach the tool, preparing whatever the attachment
   * needs on disk first.
   *
   * @return the arguments, one flag or value per element
   * @throws IOException if the attachment cannot be prepared
   */
  List<String> getArguments() throws IOException;

  /**
   * Writes the options file of a shared container virtual machine the build does not start, and
   * composes the one argument that names it.
   *
   * @param directory the directory the options file is written into
   *
   * @return the argument naming the options file
   * @throws IOException if the attachment cannot be prepared or the file cannot be written
   */
  List<String> getSharedContainerArguments(Path directory) throws IOException;
}
