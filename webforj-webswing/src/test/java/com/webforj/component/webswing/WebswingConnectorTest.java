package com.webforj.component.webswing;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.element.Element;
import com.webforj.component.webswing.event.WebswingActionEvent;
import com.webforj.component.webswing.event.WebswingInitializeEvent;
import com.webforj.component.webswing.event.WebswingStartEvent;
import com.webforj.dispatcher.EventListener;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class WebswingConnectorTest {

  @Mock
  private Element mockElement;

  private WebswingConnector connector;

  @BeforeEach
  void setUp() {
    connector = new WebswingConnector() {
      @Override
      protected Element getElement() {
        return mockElement;
      }
    };
    connector = spy(connector);
  }

  @Nested
  class Constructors {

    @Test
    void shouldCreateConnectorWithUrlAndAutoStart() {
      String url = "https://localhost:8080/webswing";
      boolean autoStart = false;

      WebswingConnector connector = new WebswingConnector(url, autoStart);

      assertEquals(url, connector.getUrl());
      assertNotNull(connector.getOptions());
      assertEquals(autoStart, connector.getOptions().isAutoStart());
    }

    @Test
    void shouldCreateConnectorWithUrlAndDefaultAutoStart() {
      String url = "https://localhost:8080/webswing";

      WebswingConnector connector = new WebswingConnector(url);

      assertEquals(url, connector.getUrl());
      assertNotNull(connector.getOptions());
      assertTrue(connector.getOptions().isAutoStart());
    }

    @Test
    void shouldCreateEmptyConnector() {
      WebswingConnector connector = new WebswingConnector();

      assertEquals("", connector.getUrl());
    }
  }

  @Nested
  class Properties {

    @Test
    void shouldSetAndGetUrl() {
      String url = "https://localhost:8080/webswing";

      connector.setUrl(url);
      assertEquals(url, connector.getUrl());
    }

    @Test
    void shouldSetAndGetOptions() {
      WebswingOptions options =
          new WebswingOptions().setAutoStart(false).setAutoReconnect(5000).setDisableLogout(true);

      connector.setOptions(options);
      assertSame(options, connector.getOptions());
    }
  }

  @Nested
  class ControlMethods {

    @Test
    void shouldCallStartOnElement() {
      connector.start();

      verify(mockElement).callJsFunctionVoidAsync("start");
    }

    @Test
    void shouldCallStopOnElement() {
      connector.stop();

      verify(mockElement).callJsFunctionVoidAsync("stop");
    }

    @Test
    void shouldCallDestroyOnElement() {
      connector.destroy();

      verify(mockElement).callJsFunction("destroy");
    }

    @Test
    void shouldCallRepaintOnElement() {
      connector.repaint();

      verify(mockElement).callJsFunctionAsync("repaint");
    }

    @Test
    void shouldCallLogoutWithParameters() {
      boolean tabLogout = true;
      boolean closeOnSuccess = false;

      connector.logout(tabLogout, closeOnSuccess);

      verify(mockElement).callJsFunctionVoidAsync("logout", tabLogout, closeOnSuccess);
    }

    @Test
    void shouldCallLogoutWithDefaultParameters() {
      connector.logout();

      verify(mockElement).callJsFunctionVoidAsync("logout", true, false);
    }
  }

  @Nested
  class InstanceMethods {

    @Test
    void shouldReturnInstanceIdWhenAvailable() {
      String expectedId = "instance-123";
      when(mockElement.callJsFunction("getInstanceId")).thenReturn(expectedId);

      Optional<String> instanceId = connector.getInstanceId();

      assertTrue(instanceId.isPresent());
      assertEquals(expectedId, instanceId.get());
    }

    @Test
    void shouldReturnEmptyOptionalWhenInstanceIdIsNull() {
      when(mockElement.callJsFunction("getInstanceId")).thenReturn(null);

      Optional<String> instanceId = connector.getInstanceId();
      assertFalse(instanceId.isPresent());
    }
  }

  @Nested
  class ActionMethods {

    @Test
    void shouldPerformActionWithAllParameters() {
      String actionName = "submit";
      String data = "{\"test\":\"data\"}";
      String binaryData = "binary";
      // Base64 encoded "binary"
      String expectedEncodedData = "YmluYXJ5";

      connector.performAction(actionName, data, binaryData);

      verify(mockElement).callJsFunctionVoidAsync("performAction", actionName, data,
          expectedEncodedData);
    }

    @Test
    void shouldPerformActionWithActionNameAndDataOnly() {
      String actionName = "submit";
      String data = "{\"test\":\"data\"}";

      connector.performAction(actionName, data);

      verify(mockElement).callJsFunctionVoidAsync("performAction", actionName, data, "");
    }

    @Test
    void shouldPerformActionWithActionNameOnly() {
      String actionName = "submit";

      connector.performAction(actionName);

      verify(mockElement).callJsFunctionVoidAsync("performAction", actionName, "", "");
    }
  }

  @Nested
  class EventListeners {

    @Test
    void shouldAddInitializedListenerViaOnInitialized() {
      EventListener<WebswingInitializeEvent> listener = event -> {
      };
      WebswingConnector realConnector = new WebswingConnector();

      realConnector.onInitialize(listener);

      assertEquals(1, realConnector.getEventListeners(WebswingInitializeEvent.class).size());
    }

    @Test
    void shouldAddStartedListenerViaOnStarted() {
      EventListener<WebswingStartEvent> listener = event -> {
      };
      WebswingConnector realConnector = new WebswingConnector();

      realConnector.onStart(listener);

      assertEquals(1, realConnector.getEventListeners(WebswingStartEvent.class).size());
    }

    @Test
    void shouldAddActionListenerViaOnAction() {
      EventListener<WebswingActionEvent> listener = event -> {
      };
      WebswingConnector realConnector = new WebswingConnector();

      realConnector.onAction(listener);

      assertEquals(1, realConnector.getEventListeners(WebswingActionEvent.class).size());
    }
  }
}
