package org.dwcj.data;

/**
 * An interface for entities that have an identifiable entity key.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public interface HasEntityKey {

  /**
   * Retrieves the key of this entity.
   *
   * @return The entity key.
   */
  Object getEntityKey();
}
