package com.webforj.testbed;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.App;
import com.webforj.BusyIndicator;
import com.webforj.Page;
import com.webforj.Request;
import com.webforj.environment.ObjectTable;
import com.webforj.environment.StringTable;
import com.webforj.environment.namespace.PrivateNamespace;
import com.webforj.webstorage.CookieStorage;
import com.webforj.webstorage.LocalStorage;
import com.webforj.webstorage.SessionStorage;
import org.junit.jupiter.api.Test;

class AppApiBehaviorTest extends TestbedTestSupport {
  private static final String OBJECT_KEY = "shared";
  private static final String COUNTER_KEY = "count";

  @Test
  void shouldDriveTheBusyIndicator() {
    BusyIndicator indicator = App.getBusyIndicator();

    indicator.setText("Loading data");
    indicator.setVisible(true);

    assertEquals("Loading data", indicator.getText());
    assertTrue(indicator.isVisible());

    indicator.setVisible(false);

    assertFalse(indicator.isVisible());
  }

  @Test
  void shouldDriveThePage() {
    Page page = Page.getCurrent();

    page.setTitle("Dashboard");

    assertEquals("Dashboard", page.getTitle());

    page.setAttribute("data-app", "under-test", "");

    assertEquals("under-test", page.getAttribute("data-app", ""));
  }

  @Test
  void shouldDriveWebStorage() {
    LocalStorage localStorage = LocalStorage.getCurrent();
    SessionStorage sessionStorage = SessionStorage.getCurrent();
    CookieStorage cookieStorage = CookieStorage.getCurrent();

    localStorage.add("key", "local");
    sessionStorage.add("key", "session");
    cookieStorage.add("key", "cookie");

    assertEquals("local", localStorage.get("key"));
    assertEquals("session", sessionStorage.get("key"));
    assertEquals("cookie", cookieStorage.get("key"));

    localStorage.remove("key");

    assertNull(localStorage.get("key"));
  }

  @Test
  void shouldDriveStringTableAndObjectTable() {
    StringTable.put("!GREETING", "hello");

    assertEquals("hello", StringTable.get("!GREETING"));

    ObjectTable.put(OBJECT_KEY, 42);

    assertTrue(ObjectTable.contains(OBJECT_KEY));
    assertEquals(42, ObjectTable.get(OBJECT_KEY));

    ObjectTable.clear(OBJECT_KEY);

    assertFalse(ObjectTable.contains(OBJECT_KEY));
  }

  @Test
  void shouldDriveNamespaces() throws Exception {
    PrivateNamespace namespace = new PrivateNamespace("test", "shared");

    namespace.put(COUNTER_KEY, 5);

    assertEquals(5, namespace.get(COUNTER_KEY));
    assertEquals(1, namespace.size());

    namespace.remove(COUNTER_KEY);

    assertEquals(0, namespace.size());
  }

  @Test
  void shouldAnswerClientFacts() {
    Request request = Request.getCurrent();

    assertEquals("127.0.0.1", request.getIPAddress());
    assertNotNull(request.getLocale());
    assertNotNull(request.getUrl());
  }
}
