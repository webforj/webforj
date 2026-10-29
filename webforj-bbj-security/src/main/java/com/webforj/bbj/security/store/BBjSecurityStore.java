package com.webforj.bbj.security.store;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.bbj.security.model.BBjSession;
import com.webforj.bbj.security.store.BBjEntryFile.Content;
import com.webforj.router.history.Location;
import com.webforj.webstorage.LocalStorage;
import java.nio.file.Path;
import java.util.Optional;

/**
 * Keeps the {@link BBjSession} of the signed in user and the location the user requested before
 * being sent to authenticate.
 *
 * <p>
 * All tabs of one browser share the session and the location, so both survive a page reload.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public interface BBjSecurityStore {

  /**
   * Creates the security store for an environment.
   *
   * <p>
   * The session is kept encrypted in the session file of the application. Its name and key are held
   * in the local storage of the browser. The location is kept in the local storage of the browser.
   * </p>
   *
   * @param environment the environment of the application
   * @return the security store
   */
  static BBjSecurityStore create(Environment environment) {
    var file = new BBjEntryFile(() -> Path.of(System.getProperty("java.io.tmpdir")),
        App::getApplicationName, Content.SESSIONS);
    return new BBjBrowserSecurityStore(file, LocalStorage::getCurrent);
  }

  /**
   * Saves the session shared by all tabs of the current browser and replaces the session saved
   * before.
   *
   * @param session the session to keep
   *
   * @throws IllegalStateException if the session cannot be kept
   */
  void saveSession(BBjSession session);

  /**
   * Reads the session saved for the current browser.
   *
   * <p>
   * A session that is expired, gone or changed is removed and reported as empty.
   * </p>
   *
   * @return the session, or empty when the browser holds none that can be used
   */
  Optional<BBjSession> readSession();

  /**
   * Removes the session saved for the current browser. This method never throws.
   */
  void clearSession();

  /**
   * Saves the requested location and replaces a location saved earlier.
   *
   * @param location the requested location
   */
  void saveLocation(Location location);

  /**
   * Reads the saved location.
   *
   * @return the saved location, or empty if none is saved
   */
  Optional<Location> readLocation();

  /**
   * Removes the saved location.
   */
  void clearLocation();

}
