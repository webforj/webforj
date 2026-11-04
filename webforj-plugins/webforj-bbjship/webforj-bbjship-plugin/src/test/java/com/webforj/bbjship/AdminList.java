package com.webforj.bbjship;

import com.basis.api.admin.client.BBjAdminClientList;
import java.util.Collection;

/**
 * Holds the entries a mocked BBjServices administrator returns as a list.
 *
 * @param <E> the entry type
 */
public final class AdminList<E> extends BBjAdminClientList<E> {

  /**
   * Creates an empty list.
   */
  public AdminList() {
    super();
  }

  /**
   * Creates a list holding the given entries.
   *
   * @param entries the entries in order
   */
  public AdminList(Collection<? extends E> entries) {
    super();
    addAll(entries);
  }
}
