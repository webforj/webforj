package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class IntervalTest {
  Interval interval;
  Environment environment;
  BBjAPI api;
  WebforjBBjBridge bridge;
  EventListener<Interval.ElapsedEvent> listener;

  @BeforeEach
  void setUp() throws BBjException {
    environment = mock(Environment.class);
    api = mock(BBjAPI.class);
    bridge = mock(WebforjBBjBridge.class);
    listener = mock(EventListener.class);

    when(environment.getBBjAPI()).thenReturn(api);
    when(environment.getWebforjHelper()).thenReturn(bridge);

    interval = spy(new Interval(1.0f, listener));
    doReturn(environment).when(interval).getEnvironment();
  }

  @Test
  void shouldSetDelay() {
    interval.setDelay(2.0f);
    assertEquals(2.0f, interval.getDelay());

    assertThrows(IllegalArgumentException.class, () -> interval.setDelay(-1.0f));
  }

  @Test
  void shouldStartInterval() throws BBjException {
    interval.start();
    assertTrue(interval.isRunning());
    verify(api, times(1)).createTimer(anyString(), any(BasisNumber.class), any(), eq("onEvent"));
  }

  @Test
  void shouldNotStartWhenAlreadyRunning() throws BBjException {
    interval.start();
    interval.start();
    verify(api, times(1)).createTimer(anyString(), any(BasisNumber.class), any(), eq("onEvent"));
  }

  @Test
  void shouldStopInterval() throws BBjException {
    interval.start();
    interval.stop();
    assertFalse(interval.isRunning());
    verify(api, times(1)).removeTimer(anyString());
  }

  @Test
  void shouldNotStopWhenNotRunning() throws BBjException {
    interval.stop();
    verify(api, times(0)).removeTimer(anyString());
  }

  @Test
  void shouldRestartInterval() throws BBjException {
    interval.start();
    interval.restart();
    verify(api, times(1)).removeTimer(anyString());
    verify(api, times(2)).createTimer(anyString(), any(BasisNumber.class), any(), eq("onEvent"));
  }

  @Test
  void shouldAddElapsedListener() {
    ListenerRegistration<Interval.ElapsedEvent> registration =
        interval.addElapsedListener(listener);
    assertNotNull(registration);
    assertEquals(2, interval.getElapsedListeners().size());
  }
}

