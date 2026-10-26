package com.webforj.testbed.runtime;

import jakarta.servlet.http.HttpSession;
import java.util.function.Consumer;

/**
 * An in memory stand in for {@link HttpSession.Accessor} backing a browserless test context.
 *
 * <p>
 * Access runs synchronously against the same {@link TestbedHttpSessionMock} for the lifetime of the
 * context, which is what {@link com.webforj.Environment#getSessionAccessor()} returns while a test
 * runs.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class TestbedHttpSessionAccessorMock implements HttpSession.Accessor {
  private final TestbedHttpSessionMock session;

  /**
   * Creates a new accessor over the given session.
   *
   * @param session the session every access reaches
   */
  TestbedHttpSessionAccessorMock(TestbedHttpSessionMock session) {
    this.session = session;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void access(Consumer<HttpSession> action) {
    session.checkValid();
    action.accept(session);
  }
}
