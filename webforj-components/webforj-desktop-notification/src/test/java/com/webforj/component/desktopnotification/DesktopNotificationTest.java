package com.webforj.component.desktopnotification;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;

import com.google.gson.Gson;
import com.webforj.Page;
import com.webforj.component.desktopnotification.event.DesktopNotificationClickEvent;
import com.webforj.component.desktopnotification.event.DesktopNotificationCloseEvent;
import com.webforj.component.desktopnotification.event.DesktopNotificationErrorEvent;
import com.webforj.component.desktopnotification.event.DesktopNotificationOpenEvent;
import com.webforj.dispatcher.ListenerRegistration;
import java.util.function.Consumer;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
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
    mockedPage.reset();
    notification = new DesktopNotification("Test Title", "Test Body");
    Page page = mock(Page.class);
    mockedPage.when(Page::getCurrent).thenReturn(page);
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

  @Test
  void shouldSendTitleBodyAndIconToClientAsJsonStrings() {
    Page page = mock(Page.class);
    mockedPage.when(() -> Page.ifPresent(any())).thenAnswer(inv -> {
      Consumer<Page> consumer = inv.getArgument(0);
      consumer.accept(page);

      return null;
    });

    String title = "It's ready";
    String body = "Total `${total}`";
    String icon = "images/`logo`.png";
    notification.setTitle(title);
    notification.setBody(body);
    notification.setIcon(icon);
    notification.open();

    ArgumentCaptor<String> script = ArgumentCaptor.forClass(String.class);
    verify(page).executeJsVoidAsync(script.capture());
    Gson gson = new Gson();
    assertTrue(script.getValue().contains("new Notification(" + gson.toJson(title) + ", {body: "
        + gson.toJson(body) + ", icon: " + gson.toJson(icon) + ", tag: '"));
    assertFalse(script.getValue().contains("'" + title + "'"));
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
