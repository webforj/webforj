package com.webforj.testbed.runtime;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import jakarta.servlet.http.HttpSession;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import org.junit.jupiter.api.Test;

class TestbedHttpSessionMockTest {

  @Test
  void shouldReportDeterministicIdentity() {
    TestbedHttpSessionMock session = new TestbedHttpSessionMock();

    assertEquals(0, session.getCreationTime());
    assertEquals(0, session.getLastAccessedTime());
    assertEquals("testbed-session", session.getId());
    assertNull(session.getServletContext());
    assertEquals(-1, session.getMaxInactiveInterval());
  }

  @Test
  void shouldIgnoreMaxInactiveInterval() {
    TestbedHttpSessionMock session = new TestbedHttpSessionMock();

    session.setMaxInactiveInterval(30);

    assertEquals(-1, session.getMaxInactiveInterval());
  }

  @Test
  void shouldStoreAndRemoveAttributes() {
    TestbedHttpSessionMock session = new TestbedHttpSessionMock();

    session.setAttribute("user", "ada");

    assertEquals("ada", session.getAttribute("user"));
    assertEquals(List.of("user"), Collections.list(session.getAttributeNames()));

    session.removeAttribute("user");

    assertNull(session.getAttribute("user"));
  }

  @Test
  void shouldTreatTheNullValueAsRemoval() {
    TestbedHttpSessionMock session = new TestbedHttpSessionMock();
    session.setAttribute("user", "ada");

    session.setAttribute("user", null);

    assertNull(session.getAttribute("user"));
  }

  @Test
  void shouldKeepTheJoinedSessionStateIndependentOfAttributes() {
    TestbedHttpSessionMock session = new TestbedHttpSessionMock();

    assertFalse(session.isNew());

    session.setAttribute("user", "ada");

    assertFalse(session.isNew());

    session.removeAttribute("user");

    assertFalse(session.isNew());

    session.setAttribute("user", "ada");
    session.setAttribute("user", null);

    assertFalse(session.isNew());
  }

  @Test
  void shouldRejectOperationsAfterInvalidation() {
    TestbedHttpSessionMock session = new TestbedHttpSessionMock();
    session.setAttribute("user", "ada");

    session.invalidate();

    assertAll(() -> assertThrows(IllegalStateException.class, session::getCreationTime),
        () -> assertThrows(IllegalStateException.class, session::getLastAccessedTime),
        () -> assertThrows(IllegalStateException.class, () -> session.getAttribute("user")),
        () -> assertThrows(IllegalStateException.class, session::getAttributeNames),
        () -> assertThrows(IllegalStateException.class,
            () -> session.setAttribute("user", "grace")),
        () -> assertThrows(IllegalStateException.class, () -> session.setAttribute("user", null)),
        () -> assertThrows(IllegalStateException.class, () -> session.removeAttribute("user")),
        () -> assertThrows(IllegalStateException.class, session::isNew),
        () -> assertThrows(IllegalStateException.class, session::invalidate),
        () -> assertThrows(IllegalStateException.class, session::getAccessor));
  }

  @Test
  void shouldAccessTheSessionThroughTheAccessor() {
    TestbedHttpSessionMock session = new TestbedHttpSessionMock();
    TestbedHttpSessionAccessorMock accessor = new TestbedHttpSessionAccessorMock(session);

    accessor.access(accessed -> accessed.setAttribute("user", "ada"));

    assertEquals("ada", session.getAttribute("user"));
  }

  @Test
  void shouldObtainAnAccessorFromTheSession() {
    TestbedHttpSessionMock session = new TestbedHttpSessionMock();
    HttpSession.Accessor accessor = session.getAccessor();

    accessor.access(accessed -> accessed.setAttribute("user", "ada"));

    assertEquals("ada", session.getAttribute("user"));
  }

  @Test
  void shouldRejectAccessorCallbacksAfterInvalidation() {
    TestbedHttpSessionMock session = new TestbedHttpSessionMock();
    HttpSession.Accessor accessor = session.getAccessor();
    HttpSession.Accessor otherAccessor = session.getAccessor();
    AtomicBoolean invoked = new AtomicBoolean();
    Consumer<HttpSession> action = accessed -> invoked.set(true);

    accessor.access(HttpSession::invalidate);

    assertThrows(IllegalStateException.class, () -> accessor.access(action));
    assertThrows(IllegalStateException.class, () -> otherAccessor.access(action));
    assertFalse(invoked.get());
    assertThrows(IllegalStateException.class, () -> session.setAttribute("user", "ada"));
    assertThrows(IllegalStateException.class, () -> session.getAttribute("user"));
  }
}
