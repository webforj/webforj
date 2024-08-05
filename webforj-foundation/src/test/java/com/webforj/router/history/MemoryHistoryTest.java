package com.webforj.router.history;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.router.history.event.HistoryStateChangeEvent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;


class MemoryHistoryTest {

  private MemoryHistory memoryHistory;
  private EventDispatcher mockDispatcher;

  @BeforeEach
  void setUp() {
    mockDispatcher = spy(EventDispatcher.class);
    memoryHistory = new MemoryHistory() {
      @Override
      EventDispatcher getDispatcher() {
        return mockDispatcher;
      }
    };
  }

  @Test
  void shouldHaveInitialSizeOfZero() {
    assertEquals(0, memoryHistory.size());
  }

  @Test
  void shouldPushState() {
    Location location = new Location("http://example.com");
    memoryHistory.pushState("state1", location);

    assertEquals(1, memoryHistory.size());

    ArgumentCaptor<HistoryStateChangeEvent> captor =
        ArgumentCaptor.forClass(HistoryStateChangeEvent.class);
    verify(mockDispatcher, times(1)).dispatchEvent(captor.capture());
    assertEquals("state1", captor.getValue().getState().get());
    assertEquals(location, captor.getValue().getLocation().get());
  }

  @Test
  void shouldReplaceState() {
    Location location1 = new Location("http://example.com/page1");
    Location location2 = new Location("http://example.com/page2");
    memoryHistory.pushState("state1", location1);
    memoryHistory.replaceState("state2", location2);

    assertEquals(1, memoryHistory.size());

    ArgumentCaptor<HistoryStateChangeEvent> captor =
        ArgumentCaptor.forClass(HistoryStateChangeEvent.class);
    verify(mockDispatcher, times(2)).dispatchEvent(captor.capture());
    assertEquals("state2", captor.getAllValues().get(1).getState().get());
    assertEquals(location2, captor.getAllValues().get(1).getLocation().get());
  }

  @Test
  void shouldNavigateBackAndForward() {
    Location location1 = new Location("http://example.com/page1");
    Location location2 = new Location("http://example.com/page2");
    memoryHistory.pushState("state1", location1);
    memoryHistory.pushState("state2", location2);

    memoryHistory.back();
    assertEquals(2, memoryHistory.size());
    verify(mockDispatcher, times(3)).dispatchEvent(any(HistoryStateChangeEvent.class));

    memoryHistory.forward();
    assertEquals(2, memoryHistory.size());
    verify(mockDispatcher, times(4)).dispatchEvent(any(HistoryStateChangeEvent.class));
  }

  @Test
  void shouldGoToSpecifiedIndex() {
    Location location1 = new Location("http://example.com/page1");
    Location location2 = new Location("http://example.com/page2");
    Location location3 = new Location("http://example.com/page3");
    memoryHistory.pushState("state1", location1);
    memoryHistory.pushState("state2", location2);
    memoryHistory.pushState("state3", location3);

    memoryHistory.go(1);
    assertEquals(3, memoryHistory.size());

    ArgumentCaptor<HistoryStateChangeEvent> captor =
        ArgumentCaptor.forClass(HistoryStateChangeEvent.class);
    verify(mockDispatcher, times(4)).dispatchEvent(captor.capture());
    assertEquals("state2", captor.getValue().getState().get());
    assertEquals(location2, captor.getValue().getLocation().get());
  }

  @Test
  void shouldRegisterAndRemoveListener() {
    EventListener<HistoryStateChangeEvent> mockListener = mock(EventListener.class);
    memoryHistory.addHistoryStateChangeListener(mockListener);

    verify(mockDispatcher, times(1)).addListener(eq(HistoryStateChangeEvent.class),
        eq(mockListener));
  }
}
