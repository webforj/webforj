package org.dwcj.webstorage;

import com.basis.bbj.proxies.BBjThinClient;
import com.basis.startup.type.BBjException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * Abstract implementation of the WebStorage interface.
 */
public abstract class AbstractWebStorage implements WebStorage {

  private final BBjThinClient thinClient;
  private final WebStorageType type;

  protected AbstractWebStorage(BBjThinClient thinClient, WebStorageType type) {
    this.thinClient = thinClient;
    this.type = type;
  }

  @Override
  public void add(String key, String value) {
    add(PropertySameSite.SAME_SITE_DEFAULT, key, value);
  }

  @Override
  public void add(Map<String, String> values) {
    add(PropertySameSite.SAME_SITE_DEFAULT, values);
  }

  protected void add(PropertySameSite samesite, String key, String value) {
    try {
      thinClient.setUserProperty(type.getValue(), samesite.getValue(), key, value);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to add.", e);
    }
  }

  protected void add(PropertySameSite samesite, Map<String, String> values) {
    try {
      thinClient.setUserProperties(type.getValue(), samesite.getValue(), values);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to add.", e);
    }
  }


  @Override
  public String get(String key) {
    try {
      return thinClient.getUserProperty(type.getValue(), key);
    } catch (BBjException e) {
      return null;
    }
  }

  @Override
  public Map<String, String> get(Collection<String> keys) {
    try {
      return thinClient.getUserProperties(type.getValue(), keys);
    } catch (BBjException e) {
      return new HashMap<>();
    }
  }

  private void remove(String key) {
    try {
      thinClient.setUserProperty(type.getValue(), key, null);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to remove key.", e);
    }
  }

  @Override
  public void remove(String... keys) {
    for (String key : keys) {
      remove(key);
    }
  }

  @Override
  public void clear() {
    try {
      thinClient.clearUserProperties(type.getValue());
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to clear web storage.", e);
    }
  }


  /**
   * The type of available web storages.
   */
  protected enum WebStorageType {
    /**
     * Maps to browser cookies with a 30-day expiration.
     */
    COOKIES(0),

    /**
     * Maps to browser local storage.
     */
    STORAGE(1),

    /**
     * Maps to browser session storage.
     */
    SESSION(-1);

    private long value;

    private WebStorageType(long value) {
      this.value = value;
    }

    /**
     * Get the value of the enum.
     *
     * @return the value of the enum
     */
    public long getValue() {
      return value;
    }
  }

  /**
   * Controls the set of domains that can read a given cookie. For more information, see Google's
   * notes for Chrome 80+.
   */
  protected enum PropertySameSite {

    /**
     * The default SameSite behavior is set to Lax.
     */
    SAME_SITE_DEFAULT(""),

    /**
     * This value provides a balance between security and usability. The cookie will not be sent in
     * cross-site requests that are initiated by "top-level" navigation (e.g., clicking a link), but
     * it will be sent in "subsequent" requests that occur within the context of the site.
     */
    SAME_SITE_LAX("Lax"),

    /**
     * This value allows the cookie to be sent in cross-site requests. However, there is an
     * additional requirement for secure transmission. It means that the cookie will only be sent if
     * the request is made over HTTPS.
     */
    SAME_SITE_NONE("None"),

    /**
     * With this value, the cookie will only be sent in requests that originate from the same site
     * (site for which the cookie was set). It will not be sent in any cross-site context, including
     * when navigating from an external site or using subdomain links.
     */
    SAME_SITE_STRICT("Strict");

    private String value;

    private PropertySameSite(String value) {
      this.value = value;
    }

    /**
     * Get the value of the enum.
     *
     * @return the value of the enum
     */
    public String getValue() {
      return this.value;
    }
  }

}
