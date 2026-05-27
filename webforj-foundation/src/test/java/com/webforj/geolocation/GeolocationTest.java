package com.webforj.geolocation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.event.BBjGeolocationEvent;
import com.basis.bbj.proxies.sysgui.BBjGeolocation;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjNumber;
import com.webforj.Environment;
import com.webforj.PendingResult;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.environment.ObjectTable;
import com.webforj.geolocation.event.GeolocationWatchEvent;
import com.webforj.geolocation.exception.WebforjGeolocationException;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class GeolocationTest {

  Geolocation geolocation;
  Environment environment;
  BBjAPI api;
  BBjSysGui sysGui;
  BBjGeolocation bbjGeolocation;
  MockedStatic<Environment> mockedEnvironment;

  @BeforeEach
  void setUp() throws BBjException {
    environment = mock(Environment.class);
    api = mock(BBjAPI.class);
    sysGui = mock(BBjSysGui.class);
    bbjGeolocation = mock(BBjGeolocation.class);

    when(environment.getBBjAPI()).thenReturn(api);
    when(environment.getSysGui()).thenReturn(sysGui);
    when(sysGui.getGeolocation()).thenReturn(bbjGeolocation);

    mockedEnvironment = mockStatic(Environment.class);
    mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);

    geolocation = spy(Geolocation.class);
    when(geolocation.getEnvironment()).thenReturn(environment);
  }

  @AfterEach
  void tearDown() {
    mockedEnvironment.close();
  }

  @Nested
  class StaticAccess {

    @Test
    void shouldReturnSingletonFromObjectTable() {
      try (MockedStatic<ObjectTable> mockedObjectTable = mockStatic(ObjectTable.class)) {
        Geolocation existing = mock(Geolocation.class);
        mockedObjectTable.when(() -> ObjectTable.contains(Geolocation.class.getName()))
            .thenReturn(true);
        mockedObjectTable.when(() -> ObjectTable.get(Geolocation.class.getName()))
            .thenReturn(existing);

        assertSame(existing, Geolocation.getCurrent());
      }
    }

    @Test
    void shouldCreateInstanceWhenNotInObjectTable() {
      try (MockedStatic<ObjectTable> mockedObjectTable = mockStatic(ObjectTable.class)) {
        mockedObjectTable.when(() -> ObjectTable.contains(Geolocation.class.getName()))
            .thenReturn(false);

        Geolocation instance = Geolocation.getCurrent();

        assertNotNull(instance);
        mockedObjectTable
            .verify(() -> ObjectTable.put(eq(Geolocation.class.getName()), any(Geolocation.class)));
      }
    }

    @Test
    void ifPresentShouldInvokeConsumer() {
      try (MockedStatic<ObjectTable> mockedObjectTable = mockStatic(ObjectTable.class)) {
        mockedObjectTable.when(() -> ObjectTable.contains(Geolocation.class.getName()))
            .thenReturn(false);

        AtomicReference<Geolocation> captured = new AtomicReference<>();
        Geolocation.ifPresent(captured::set);

        assertNotNull(captured.get());
      }
    }
  }

  @Nested
  class Configuration {

    @Test
    void shouldSetHighAccuracy() {
      geolocation.useHighAccuracy(true);
      verify(bbjGeolocation).setHighAccuracy(true);
    }

    @Test
    void shouldGetHighAccuracy() {
      when(bbjGeolocation.isHighAccuracy()).thenReturn(true);
      assertTrue(geolocation.isHighAccuracy());
    }

    @Test
    void shouldSetTimeout() {
      geolocation.useTimeout(15);
      verify(bbjGeolocation).setTimeout(any(BBjNumber.class));
    }

    @Test
    void shouldGetTimeout() {
      BBjNumber number = mock(BBjNumber.class);
      when(number.doubleValue()).thenReturn(12.5);
      when(bbjGeolocation.getTimeout()).thenReturn(number);

      assertEquals(12.5, geolocation.getTimeout());
    }

    @Test
    void shouldSetMaximumAge() {
      geolocation.useMaximumAge(60);
      verify(bbjGeolocation).setMaximumAge(any(BBjNumber.class));
    }

    @Test
    void shouldGetMaximumAge() {
      BBjNumber number = mock(BBjNumber.class);
      when(number.doubleValue()).thenReturn(90.0);
      when(bbjGeolocation.getMaximumAge()).thenReturn(number);

      assertEquals(90.0, geolocation.getMaximumAge());
    }
  }

  @Nested
  class GetCurrentPosition {

    @Test
    void shouldRegisterCallbackAndRequestPosition() throws BBjException {
      PendingResult<GeolocationPosition> result = geolocation.getCurrentPosition();

      assertNotNull(result);
      assertFalse(result.isDone());
      verify(bbjGeolocation, times(1)).setCallback(eq(SysGuiEventConstants.ON_GEOLOCATION_POSITION),
          any(), eq("handlePositionEvent"));
      verify(bbjGeolocation, times(1)).getCurrentPosition();
    }

    @Test
    void shouldRegisterCallbackOnlyOnceAcrossMultipleRequests() throws BBjException {
      geolocation.getCurrentPosition();
      geolocation.getCurrentPosition();
      geolocation.getCurrentPosition();

      verify(bbjGeolocation, times(1)).setCallback(eq(SysGuiEventConstants.ON_GEOLOCATION_POSITION),
          any(), eq("handlePositionEvent"));
      verify(bbjGeolocation, times(3)).getCurrentPosition();
    }
  }

  @Nested
  class WatchListener {

    @Test
    void shouldRegisterWatchCallbackWhenFirstListenerAdded() throws BBjException {
      ListenerRegistration<GeolocationWatchEvent> registration = geolocation.onWatch(event -> {
      });

      assertNotNull(registration);
      verify(bbjGeolocation, times(1)).setCallback(eq(SysGuiEventConstants.ON_GEOLOCATION_WATCH),
          any(), eq("handleEvent"));
    }

    @Test
    void shouldRegisterWatchCallbackOnlyOnceForMultipleListeners() throws BBjException {
      geolocation.onWatch(event -> {
      });
      geolocation.onWatch(event -> {
      });

      verify(bbjGeolocation, times(1)).setCallback(eq(SysGuiEventConstants.ON_GEOLOCATION_WATCH),
          any(), eq("handleEvent"));
    }

    @Test
    void shouldClearWatchCallbackWhenLastListenerRemoved() throws BBjException {
      ListenerRegistration<GeolocationWatchEvent> reg1 = geolocation.onWatch(event -> {
      });
      ListenerRegistration<GeolocationWatchEvent> reg2 = geolocation.onWatch(event -> {
      });

      reg1.remove();
      verify(bbjGeolocation, never()).clearCallback(anyInt());

      reg2.remove();
      verify(bbjGeolocation, times(1)).clearCallback(SysGuiEventConstants.ON_GEOLOCATION_WATCH);

      // registering after all removed should set callback again
      geolocation.onWatch(event -> {
      });
      verify(bbjGeolocation, times(2)).setCallback(eq(SysGuiEventConstants.ON_GEOLOCATION_WATCH),
          any(), eq("handleEvent"));
    }
  }

  @Nested
  class HandleEvent {

    @Test
    void shouldCompletePendingResultOnSuccess() {
      double latitude = 35.150036;
      double longitude = -106.593957;
      double accuracy = 10.0;
      long timestamp = 1700000000000L;
      double altitude = 1500.5;
      double altitudeAccuracy = 5.0;
      double heading = 90.0;
      double speed = 2.5;

      BBjGeolocationEvent bbjEvent = mock(BBjGeolocationEvent.class);
      when(bbjEvent.getStatus()).thenReturn(0);
      when(bbjEvent.getLatitude()).thenReturn(latitude);
      when(bbjEvent.getLongitude()).thenReturn(longitude);
      when(bbjEvent.getAccuracy()).thenReturn(accuracy);
      when(bbjEvent.getTimestamp()).thenReturn(timestamp);
      when(bbjEvent.getAltitude()).thenReturn(altitude);
      when(bbjEvent.getAltitudeAccuracy()).thenReturn(altitudeAccuracy);
      when(bbjEvent.getHeading()).thenReturn(heading);
      when(bbjEvent.getSpeed()).thenReturn(speed);

      PendingResult<GeolocationPosition> result = geolocation.getCurrentPosition();
      geolocation.getEventHandler().handlePositionEvent(bbjEvent);

      assertTrue(result.isDone());
      AtomicReference<GeolocationPosition> captured = new AtomicReference<>();
      result.thenAccept(captured::set);

      GeolocationPosition position = captured.get();
      assertEquals(latitude, position.getLatitude());
      assertEquals(longitude, position.getLongitude());
      assertEquals(accuracy, position.getAccuracy());
      assertEquals(timestamp, position.getTimestamp());
      assertEquals(altitude, position.getAltitude().orElseThrow());
      assertEquals(altitudeAccuracy, position.getAltitudeAccuracy().orElseThrow());
      assertEquals(heading, position.getHeading().orElseThrow());
      assertEquals(speed, position.getSpeed().orElseThrow());
    }

    @Test
    void shouldCompleteExceptionallyOnError() {
      String message = "User denied geolocation prompt";

      BBjGeolocationEvent bbjEvent = mock(BBjGeolocationEvent.class);
      when(bbjEvent.getStatus()).thenReturn(GeolocationStatus.PERMISSION_DENIED.getCode());
      when(bbjEvent.getMessage()).thenReturn(message);

      PendingResult<GeolocationPosition> result = geolocation.getCurrentPosition();
      geolocation.getEventHandler().handlePositionEvent(bbjEvent);

      assertTrue(result.isCompletedExceptionally());
      AtomicReference<Throwable> captured = new AtomicReference<>();
      result.exceptionally(throwable -> {
        captured.set(throwable);

        return null;
      });

      WebforjGeolocationException error = (WebforjGeolocationException) captured.get();
      assertEquals(GeolocationStatus.PERMISSION_DENIED, error.getStatus());
      assertEquals(message, error.getMessage());
    }

    @Test
    void shouldDispatchWatchEventOnSuccess() {
      double latitude = 10.0;

      AtomicReference<GeolocationWatchEvent> captured = new AtomicReference<>();
      EventListener<GeolocationWatchEvent> listener = captured::set;
      geolocation.onWatch(listener);

      BBjGeolocationEvent bbjEvent = mock(BBjGeolocationEvent.class);
      when(bbjEvent.getStatus()).thenReturn(GeolocationStatus.SUCCESS.getCode());
      when(bbjEvent.getLatitude()).thenReturn(latitude);
      when(bbjEvent.getLongitude()).thenReturn(20.0);
      when(bbjEvent.getAccuracy()).thenReturn(1.0);
      when(bbjEvent.getTimestamp()).thenReturn(42L);

      geolocation.getWatchSink().handleEvent(bbjEvent);

      assertNotNull(captured.get());
      assertTrue(captured.get().isSuccess());
      assertEquals(GeolocationStatus.SUCCESS, captured.get().getStatus());
      assertEquals(latitude, captured.get().getPosition().orElseThrow().getLatitude());
    }

    @Test
    void shouldDispatchWatchEventOnError() {
      String message = "Timed out";

      AtomicReference<GeolocationWatchEvent> captured = new AtomicReference<>();
      geolocation.onWatch(captured::set);

      BBjGeolocationEvent bbjEvent = mock(BBjGeolocationEvent.class);
      when(bbjEvent.getStatus()).thenReturn(GeolocationStatus.TIMEOUT.getCode());
      when(bbjEvent.getMessage()).thenReturn(message);

      geolocation.getWatchSink().handleEvent(bbjEvent);

      assertEquals(GeolocationStatus.TIMEOUT, captured.get().getStatus());
      assertFalse(captured.get().isSuccess());
      assertEquals(message, captured.get().getMessage().orElseThrow());
      assertTrue(captured.get().getPosition().isEmpty());
    }
  }

  @Nested
  class Destroy {

    @Test
    void shouldRemoveAllListeners() throws BBjException {
      geolocation.onWatch(event -> {
      });

      geolocation.destroy();

      verify(bbjGeolocation, never()).clearCallback(anyInt());
    }

    @Test
    void shouldBeIdempotent() throws BBjException {
      geolocation.onWatch(event -> {
      });
      geolocation.getCurrentPosition();

      geolocation.destroy();
      geolocation.destroy();

      verify(bbjGeolocation, never()).clearCallback(anyInt());
    }
  }
}
