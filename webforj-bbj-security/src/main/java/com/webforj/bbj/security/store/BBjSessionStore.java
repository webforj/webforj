package com.webforj.bbj.security.store;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.bbj.security.model.BBjSession;
import com.webforj.webstorage.SessionStorage;
import java.nio.file.Path;
import java.util.Optional;

/**
 * Keeps the {@link BBjSession} of a signed in tab, so that it survives a page reload.
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public interface BBjSessionStore {

  /**
   * Creates the session store for an environment.
   *
   * <p>
   * When the environment has an HTTP session, the session is kept in the HTTP session. Otherwise it
   * is kept encrypted in a file in the temporary directory, opened by a key held in the session
   * storage of the tab.
   * </p>
   *
   * @param environment the environment of the application
   * @return the session store
   */
  static BBjSessionStore create(Environment environment) {
    if (environment.getSessionAccessor().isPresent()) {
      return new BBjHttpSessionStore();
    }

    return new BBjFileSessionStore(() -> Path.of(System.getProperty("java.io.tmpdir")),
        App::getApplicationName, SessionStorage::getCurrent);
  }

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
