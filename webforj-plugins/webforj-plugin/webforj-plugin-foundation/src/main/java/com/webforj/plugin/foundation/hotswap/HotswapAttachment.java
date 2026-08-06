package com.webforj.plugin.foundation.hotswap;

import java.io.IOException;
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
}
