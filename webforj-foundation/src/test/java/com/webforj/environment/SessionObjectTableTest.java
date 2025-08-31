package com.webforj.environment;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.startup.type.BBjException;
import com.webforj.Environment;
import com.webforj.bridge.WebforjBBjBridge;
import jakarta.servlet.http.HttpSession;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.function.Consumer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

class SessionObjectTableTest {

  private HttpSession mockSession;
  private HttpSession.Accessor mockAccessor;
  private BBjAPI mockApi;
  private Map<String, Object> channelData;

  private final String testKey = "testKey";
  private final Object testValue = "testValue";
  private final String nonExistentKey = "nonExistentKey";

  @SuppressWarnings("unchecked")
  @BeforeEach
  void setUp() throws BBjException {
    mockSession = mock(HttpSession.class);
    mockAccessor = mock(HttpSession.Accessor.class);
    mockApi = mock(BBjAPI.class);
    channelData = new HashMap<>();

    // Setup the accessor to invoke the consumer with the mock session
    doAnswer(invocation -> {
      Consumer<HttpSession> consumer = invocation.getArgument(0);
      consumer.accept(mockSession);
      return null;
    }).when(mockAccessor).access(any(Consumer.class));

    // Setup channel data with the accessor
    channelData.put("session.accessor", mockAccessor);
    when(mockApi.getChannelData()).thenReturn(channelData);

    // Initialize environment
    Environment.init(mockApi, mock(WebforjBBjBridge.class), 0);
  }

  @AfterEach
  void tearDown() {
    Environment.cleanup();
  }

  @Test
  void shouldPutValue() {
    Object result = SessionObjectTable.put(testKey, testValue);

    assertEquals(testValue, result);

    // Verify the session setAttribute was called
    @SuppressWarnings("unchecked")
    ArgumentCaptor<Consumer<HttpSession>> captor = ArgumentCaptor.forClass(Consumer.class);
    verify(mockAccessor).access(captor.capture());
    verify(mockSession).setAttribute(testKey, testValue);
  }

  @Test
  void shouldGetValue() {
    when(mockSession.getAttribute(testKey)).thenReturn(testValue);

    Object actualValue = SessionObjectTable.get(testKey);

    assertEquals(testValue, actualValue);
    verify(mockSession).getAttribute(testKey);
  }

  @Test
  void shouldThrowExceptionWhenKeyNotFound() {
    when(mockSession.getAttribute(nonExistentKey)).thenReturn(null);
    assertThrows(NoSuchElementException.class, () -> SessionObjectTable.get(nonExistentKey));
  }

  @Test
  void shouldThrowExceptionWhenNoSessionAvailable() throws BBjException {
    channelData.clear();

    assertThrows(IllegalStateException.class, () -> SessionObjectTable.put(testKey, testValue));
    assertThrows(IllegalStateException.class, () -> SessionObjectTable.get(testKey));
  }

  @Test
  void shouldReturnTrueIfKeyExists() {
    when(mockSession.getAttribute(testKey)).thenReturn(testValue);

    assertTrue(SessionObjectTable.contains(testKey));
    verify(mockSession).getAttribute(testKey);
  }

  @Test
  void shouldReturnFalseIfKeyNotFound() {
    when(mockSession.getAttribute(nonExistentKey)).thenReturn(null);

    assertFalse(SessionObjectTable.contains(nonExistentKey));
    verify(mockSession).getAttribute(nonExistentKey);
  }

  @Test
  void shouldReturnFalseWhenNoSessionAvailable() throws BBjException {
    channelData.clear();
    assertFalse(SessionObjectTable.contains(testKey));
  }

  @SuppressWarnings("unchecked")
  @Test
  void shouldClearValueFromSession() {
    SessionObjectTable.clear(testKey);

    verify(mockAccessor).access(any(Consumer.class));
    verify(mockSession).removeAttribute(testKey);
  }

  @Test
  void shouldNotThrowWhenClearingWithNoSession() throws BBjException {
    channelData.clear();
    SessionObjectTable.clear(testKey);

    verify(mockSession, never()).removeAttribute(testKey);
  }

  @Test
  void shouldHandleNoEnvironment() {
    Environment.cleanup();

    assertThrows(IllegalStateException.class, () -> SessionObjectTable.put(testKey, testValue));
    assertThrows(IllegalStateException.class, () -> SessionObjectTable.get(testKey));
    assertFalse(SessionObjectTable.contains(testKey));

    // clear should not throw
    SessionObjectTable.clear(testKey);
  }

  @Test
  void shouldReturnCorrectSize() {
    // Setup mock attribute names
    List<String> attributeNames = new ArrayList<>();
    when(mockSession.getAttributeNames())
        .thenAnswer(invocation -> Collections.enumeration(attributeNames));

    // Initially empty
    assertEquals(0, SessionObjectTable.size());

    // Add some items (we need to track them in our test list)
    doAnswer(invocation -> {
      String key = invocation.getArgument(0);
      if (!attributeNames.contains(key)) {
        attributeNames.add(key);
      }
      return null;
    }).when(mockSession).setAttribute(any(String.class), any());

    doAnswer(invocation -> {
      String key = invocation.getArgument(0);
      attributeNames.remove(key);
      return null;
    }).when(mockSession).removeAttribute(any(String.class));

    SessionObjectTable.put("key1", "value1");
    SessionObjectTable.put("key2", "value2");
    SessionObjectTable.put("key3", "value3");

    assertEquals(3, SessionObjectTable.size());

    // Clear one item
    SessionObjectTable.clear("key2");
    assertEquals(2, SessionObjectTable.size());
  }

  @Test
  void shouldHandleBbjException() throws BBjException {
    when(mockApi.getChannelData()).thenThrow(new BBjException("Test exception", 0));

    assertThrows(IllegalStateException.class, () -> SessionObjectTable.put(testKey, testValue));
    assertThrows(IllegalStateException.class, () -> SessionObjectTable.get(testKey));
    assertFalse(SessionObjectTable.contains(testKey));
  }

  @Test
  void shouldHandleNonAccessorInChannelData() throws BBjException {
    // Put something else in channel data instead of accessor
    channelData.put("session.accessor", "not an accessor");

    assertThrows(IllegalStateException.class, () -> SessionObjectTable.put(testKey, testValue));
    assertThrows(IllegalStateException.class, () -> SessionObjectTable.get(testKey));
    assertFalse(SessionObjectTable.contains(testKey));
  }
}
