package com.webforj.devtools.craftforj.history;

import java.io.IOException;

/**
 * Thrown when another writer held the journal for longer than a call may wait.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class HistoryBusyException extends IOException {

  private static final long serialVersionUID = 1L;

  HistoryBusyException() {
    super("The history is held by another writer");
  }
}
