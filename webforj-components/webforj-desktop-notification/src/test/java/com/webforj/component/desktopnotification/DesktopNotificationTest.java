package com.webforj.component.desktopnotification;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;

import com.webforj.Page;
import com.webforj.component.desktopnotification.event.DesktopNotificationClickEvent;
import com.webforj.component.desktopnotification.event.DesktopNotificationCloseEvent;
import com.webforj.component.desktopnotification.event.DesktopNotificationErrorEvent;
import com.webforj.component.desktopnotification.event.DesktopNotificationOpenEvent;
import com.webforj.dispatcher.ListenerRegistration;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class DesktopNotificationTest {

  private DesktopNotification notification;
  private static MockedStatic<Page> mockedPage;

  @BeforeAll
  static void setup() {
    mockedPage = mockStatic(Page.class);
  }

  @AfterAll
  static void teardown() {
    mockedPage.close();
  }

  @BeforeEach
  void setUp() {
    notification = new DesktopNotification("Test Title", "Test Body");
    mockedPage.when(Page::getCurrent).thenReturn(mock(Page.class));
  }

  @Test
  void shouldSetTitle() {
    String title = "New Title";
    notification.setTitle(title);
    assertEquals(title, notification.getTitle());
  }

  @Test
  void shouldSetBody() {
    String body = "New Body";
    notification.setBody(body);
    assertEquals(body, notification.getBody());
  }

  @Test
  void shouldSetIcon() {
    String icon = "icons://new-icon.png";
    notification.setIcon(icon);
    assertEquals(icon, notification.getIcon());
  }

  @Nested
  class ShowNotification {
    @Test
    void shouldShowNotificationWithBody() {
      String body = "Body";
      DesktopNotification result = DesktopNotification.show(body);
      assertNotNull(result);
      assertEquals(body, result.getBody());
    }

    @Test
    void shouldShowNotificationWithBodyAndErrorListener() {
      String body = "Body";
      DesktopNotification result = DesktopNotification.show(body, event -> {
      });
      assertNotNull(result);
      assertEquals(body, result.getBody());
      assertTrue(result.getEventDispatcher().getCount(DesktopNotificationErrorEvent.class) > 0);
    }

    @Test
    void shouldShowNotificationWithTitleBody() {
      String title = "Title";
      String body = "Body";
      DesktopNotification result = DesktopNotification.show(title, body);
      assertNotNull(result);
      assertEquals(title, result.getTitle());
      assertEquals(body, result.getBody());
    }

    @Test
    void shouldShowNotificationWithTitleBodyAndErrorListener() {
      String title = "Title";
      String body = "Body";
      DesktopNotification result = DesktopNotification.show(title, body, event -> {
      });
      assertNotNull(result);
      assertEquals(title, result.getTitle());
      assertEquals(body, result.getBody());
      assertTrue(result.getEventDispatcher().getCount(DesktopNotificationErrorEvent.class) > 0);
    }

    @Test
    void shouldShowNotificationWithTitleBodyIcon() {
      String title = "Title";
      String body = "Body";
      String icon = "icons://icon.png";
      DesktopNotification result = DesktopNotification.show(title, body, icon);
      assertNotNull(result);
      assertEquals(title, result.getTitle());
      assertEquals(body, result.getBody());
      assertEquals(icon, result.getIcon());
    }

    @Test
    void shouldShowNotificationWithTitleBodyIconAndErrorListener() {
      String title = "Title";
      String body = "Body";
      String icon = "icons://icon.png";
      DesktopNotification result = DesktopNotification.show(title, body, icon, event -> {
      });
      assertNotNull(result);
      assertEquals(title, result.getTitle());
      assertEquals(body, result.getBody());
      assertEquals(icon, result.getIcon());
      assertTrue(result.getEventDispatcher().getCount(DesktopNotificationErrorEvent.class) > 0);
    }
  }

  @Nested
  class Listeners {
    @Test
    void shouldAddOpenListener() {
      ListenerRegistration<DesktopNotificationOpenEvent> registration =
          notification.addOpenListener(event -> {
          });
      assertNotNull(registration);
    }

    @Test
    void shouldAddCloseListener() {
      ListenerRegistration<DesktopNotificationCloseEvent> registration =
          notification.addCloseListener(event -> {
          });
      assertNotNull(registration);
    }

    @Test
    void shouldAddErrorListener() {
      ListenerRegistration<DesktopNotificationErrorEvent> registration =
          notification.addErrorListener(event -> {
          });
      assertNotNull(registration);
    }

    @Test
    void shouldAddClickListener() {
      ListenerRegistration<DesktopNotificationClickEvent> registration =
          notification.addClickListener(event -> {
          });
      assertNotNull(registration);
    }
  }
}
