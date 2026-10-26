package com.webforj.bbj.security;

import com.webforj.bbj.security.model.BBjSession;
import java.util.Optional;

/**
 * Keeps the {@link BBjSession} of a signed in tab, so that it survives a page reload.
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public interface BBjSessionStore {

  /**
   * Saves the session for the current tab and replaces the session saved before.
   *
   * @param session the session to keep
   *
   * @throws IllegalStateException if the session cannot be kept
   */
  void saveSession(BBjSession session);

  /**
   * Reads the session saved for the current tab.
   *
   * <p>
   * A session that is expired, gone or changed is removed and reported as empty.
   * </p>
   *
   * @return the session, or empty when the tab holds none that can be used
   */
  Optional<BBjSession> readSession();

  /**
   * Removes the session saved for the current tab. This method never throws.
   */
  void clearSession();

}
