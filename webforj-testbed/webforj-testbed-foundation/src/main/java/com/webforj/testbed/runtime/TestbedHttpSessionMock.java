package com.webforj.testbed.runtime;

import jakarta.servlet.ServletContext;
import jakarta.servlet.http.HttpSession;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * An in memory stand in for the HTTP session of a browserless test context.
 *
 * <p>
 * Route security and any other code reading {@link com.webforj.Environment#getSessionAccessor()}
 * reads and writes attributes through this session, so state stored in the session, such as the
 * location requested before an authentication redirect, survives across navigations within the same
 * test context exactly as it does behind a real HTTP session.
 * </p>
 *
 * <p>
 * The context represents a client that has already joined its session, so {@link #isNew()} returns
 * {@code false} regardless of the stored attributes.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class TestbedHttpSessionMock implements HttpSession {
  private final Map<String, Object> attributes = new ConcurrentHashMap<>();
  private boolean invalidated;

  /**
   * {@inheritDoc}
   */
  @Override
  public long getCreationTime() {
    checkValid();
    return 0;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getId() {
    return "testbed-session";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public long getLastAccessedTime() {
    return getCreationTime();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ServletContext getServletContext() {
    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setMaxInactiveInterval(int interval) {
    // A browserless test session never expires on its own.
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getMaxInactiveInterval() {
    return -1;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Object getAttribute(String name) {
    checkValid();
    return attributes.get(name);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Enumeration<String> getAttributeNames() {
    checkValid();
    return Collections.enumeration(attributes.keySet());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setAttribute(String name, Object value) {
    checkValid();
    if (value == null) {
      attributes.remove(name);

      return;
    }

    attributes.put(name, value);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeAttribute(String name) {
    checkValid();
    attributes.remove(name);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void invalidate() {
    checkValid();
    attributes.clear();
    invalidated = true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isNew() {
    checkValid();
    return false;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public HttpSession.Accessor getAccessor() {
    checkValid();
    return new TestbedHttpSessionAccessorMock(this);
  }

  /** Rejects operations on an invalidated session. */
  void checkValid() {
    if (invalidated) {
      throw new IllegalStateException("The HTTP session has been invalidated");
    }
  }
}
