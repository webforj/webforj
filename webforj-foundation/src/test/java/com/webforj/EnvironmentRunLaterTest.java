package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjSysGui;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.CustomObject;
import com.webforj.bridge.WebforjBBjBridge;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.stubbing.Answer;

@ExtendWith(MockitoExtension.class)
class EnvironmentRunLaterTest {

  @Mock
  private BBjAPI mockApi;

  @Mock
  private WebforjBBjBridge mockBridge;

  @Mock
  private BBjSysGui mockSysGui;

  @Mock
  private CustomObject mockEventProxy;

  private Environment environment;
  private ConcurrentHashMap<String, String> postedEvents = new ConcurrentHashMap<>();

  @BeforeEach
  void setUp() throws BBjException {
    when(mockApi.openSysGui(anyString())).thenReturn(mockSysGui);
    when(mockBridge.getEventProxy(any(), anyString())).thenReturn(mockEventProxy);

    // Capture posted custom events (lenient to avoid stubbing errors)
    lenient().doAnswer(new Answer<Void>() {
      public Void answer(InvocationOnMock invocation) throws Throwable {
        String eventName = (String) invocation.getArguments()[0];
        String payload = (String) invocation.getArguments()[1];
        postedEvents.put(eventName, payload);
        return null;
      }
    }).when(mockApi).postCustomEvent(anyString(), anyString());

    Environment.init(mockApi, mockBridge, 0);
    environment = Environment.getCurrent();
  }

  @AfterEach
  void tearDown() {
    postedEvents.clear();
    try {
      Environment.cleanup();
    } catch (Exception e) {
      // Ignore cleanup errors
    }
  }

  @Test
  void shouldExecuteImmediatelyFromMainThread() {
    AtomicBoolean executed = new AtomicBoolean(false);
    AtomicReference<Thread> executionThread = new AtomicReference<>();

    // Call from main thread (UI thread)
    PendingResult<Void> result = Environment.runLater(() -> {
      executed.set(true);
      executionThread.set(Thread.currentThread());
    });

    // Should execute immediately in the same thread
    assertTrue(executed.get(), "Should execute immediately in main thread");
    assertEquals(Thread.currentThread(), executionThread.get(), "Should execute in same thread");
    assertNotNull(result, "Should return PendingResult");
    assertTrue(result.isDone(), "Result should be immediately done");
  }

  @Test
  void shouldReturnValueFromSupplier() {
    String expectedValue = "test-value";
    AtomicReference<String> actualValue = new AtomicReference<>();

    PendingResult<String> result = Environment.runLater(() -> expectedValue);

    assertTrue(result.isDone());
    result.thenAccept(actualValue::set);
    assertEquals(expectedValue, actualValue.get());
  }

  @Test
  void shouldHandleExceptionInSupplier() {
    RuntimeException testException = new RuntimeException("Test exception");
    AtomicReference<Throwable> capturedError = new AtomicReference<>();

    PendingResult<String> result = Environment.runLater(() -> {
      throw testException;
    });

    assertTrue(result.isDone(), "Result should be done");
    assertTrue(result.isCompletedExceptionally(), "Result should be completed exceptionally");

    result.exceptionally(ex -> {
      capturedError.set(ex);
      return null;
    });

    assertEquals(testException, capturedError.get());
  }

  @Test
  void shouldPostCustomEventAndStoreRequestFromBackgroundThread() throws Exception {
    CountDownLatch eventPosted = new CountDownLatch(1);
    AtomicReference<String> capturedRequestId = new AtomicReference<>();
    AtomicReference<PendingResult<String>> resultRef = new AtomicReference<>();

    // Override the postCustomEvent to also trigger the latch
    doAnswer(new Answer<Void>() {
      public Void answer(InvocationOnMock invocation) throws Throwable {
        String eventName = (String) invocation.getArguments()[0];
        String payload = (String) invocation.getArguments()[1];
        postedEvents.put(eventName, payload);
        capturedRequestId.set(payload);
        eventPosted.countDown();
        return null;
      }
    }).when(mockApi).postCustomEvent(anyString(), anyString());

    Thread backgroundThread = new Thread(() -> {
      resultRef.set(Environment.runLater(() -> "background"));
    });

    backgroundThread.start();
    assertTrue(eventPosted.await(2, TimeUnit.SECONDS), "Event should be posted");
    backgroundThread.join();

    // Verify custom event was posted
    assertEquals(Environment.RUN_LATER_EVENT, postedEvents.keySet().iterator().next());
    String requestId = capturedRequestId.get();
    assertNotNull(requestId);

    // Verify result is not done (waiting for event processing)
    assertFalse(resultRef.get().isDone(), "Result should not be done until event is processed");
  }

  @Test
  void shouldProcessRunLaterRequestWhenEventIsHandled() throws Exception {
    // First, setup a background thread to post a runLater request
    CountDownLatch requestPosted = new CountDownLatch(1);
    AtomicReference<String> requestIdRef = new AtomicReference<>();
    AtomicBoolean supplierExecuted = new AtomicBoolean(false);
    AtomicReference<PendingResult<String>> resultRef = new AtomicReference<>();

    doAnswer(new Answer<Void>() {
      public Void answer(InvocationOnMock invocation) throws Throwable {
        requestIdRef.set((String) invocation.getArguments()[1]);
        requestPosted.countDown();
        return null;
      }
    }).when(mockApi).postCustomEvent(eq(Environment.RUN_LATER_EVENT), anyString());

    Thread backgroundThread = new Thread(() -> {
      resultRef.set(Environment.runLater(() -> {
        supplierExecuted.set(true);
        return "test-result";
      }));
    });

    backgroundThread.start();
    assertTrue(requestPosted.await(2, TimeUnit.SECONDS), "Request should be posted");
    backgroundThread.join();

    String requestId = requestIdRef.get();
    assertNotNull(requestId);
    assertFalse(supplierExecuted.get(), "Supplier should not execute until event is processed");

    // Now simulate the event handler processing the request
    environment.processRunLaterRequest(requestId);

    // Verify supplier was executed
    assertTrue(supplierExecuted.get(), "Supplier should be executed by processRunLaterRequest");

    // Verify result is completed
    PendingResult<String> result = resultRef.get();
    assertTrue(result.isDone(), "Result should be done after processing");

    AtomicReference<String> actualValue = new AtomicReference<>();
    result.thenAccept(actualValue::set);
    assertEquals("test-result", actualValue.get(), "Result should contain supplier's return value");
  }

  @Test
  void shouldCancelPendingRequestsOnCleanup() throws Exception {
    CountDownLatch requestPosted = new CountDownLatch(1);
    AtomicReference<PendingResult<String>> resultRef = new AtomicReference<>();
    AtomicReference<String> requestIdRef = new AtomicReference<>();

    doAnswer(new Answer<Void>() {
      public Void answer(InvocationOnMock invocation) throws Throwable {
        requestIdRef.set((String) invocation.getArguments()[1]);
        requestPosted.countDown();
        return null;
      }
    }).when(mockApi).postCustomEvent(eq(Environment.RUN_LATER_EVENT), anyString());

    Thread backgroundThread = new Thread(() -> {
      resultRef.set(Environment.runLater(() -> "test"));
    });

    backgroundThread.start();
    assertTrue(requestPosted.await(2, TimeUnit.SECONDS), "Request should be posted");
    backgroundThread.join();

    PendingResult<String> result = resultRef.get();
    assertNotNull(result);
    assertFalse(result.isDone(), "Result should not be done before cleanup");

    // Clean up environment which should cancel pending requests
    Environment.cleanup();

    assertTrue(result.isCancelled(), "Pending request should be cancelled on cleanup");
  }

  @Test
  void shouldHandleExceptionInProcessRunLaterRequest() throws Exception {
    // Setup a request that will throw an exception
    CountDownLatch requestPosted = new CountDownLatch(1);
    AtomicReference<String> requestIdRef = new AtomicReference<>();
    AtomicReference<PendingResult<String>> resultRef = new AtomicReference<>();
    RuntimeException testException = new RuntimeException("Test exception");

    doAnswer(new Answer<Void>() {
      public Void answer(InvocationOnMock invocation) throws Throwable {
        requestIdRef.set((String) invocation.getArguments()[1]);
        requestPosted.countDown();
        return null;
      }
    }).when(mockApi).postCustomEvent(eq(Environment.RUN_LATER_EVENT), anyString());

    Thread backgroundThread = new Thread(() -> {
      resultRef.set(Environment.runLater(() -> {
        throw testException;
      }));
    });

    backgroundThread.start();
    assertTrue(requestPosted.await(2, TimeUnit.SECONDS), "Request should be posted");
    backgroundThread.join();

    // Process the request which should handle the exception
    environment.processRunLaterRequest(requestIdRef.get());

    PendingResult<String> result = resultRef.get();
    assertTrue(result.isDone(), "Result should be done");
    assertTrue(result.isCompletedExceptionally(), "Result should be completed exceptionally");

    AtomicReference<Throwable> capturedError = new AtomicReference<>();
    result.exceptionally(ex -> {
      capturedError.set(ex);
      return null;
    });

    assertEquals(testException, capturedError.get(), "Should capture the thrown exception");
  }

  @Test
  void shouldNotProcessCancelledRequest() throws Exception {
    CountDownLatch requestPosted = new CountDownLatch(1);
    AtomicReference<String> requestIdRef = new AtomicReference<>();
    AtomicReference<PendingResult<String>> resultRef = new AtomicReference<>();
    AtomicBoolean supplierExecuted = new AtomicBoolean(false);

    doAnswer(new Answer<Void>() {
      public Void answer(InvocationOnMock invocation) throws Throwable {
        requestIdRef.set((String) invocation.getArguments()[1]);
        requestPosted.countDown();
        return null;
      }
    }).when(mockApi).postCustomEvent(eq(Environment.RUN_LATER_EVENT), anyString());

    Thread backgroundThread = new Thread(() -> {
      resultRef.set(Environment.runLater(() -> {
        supplierExecuted.set(true);
        return "should-not-execute";
      }));
    });

    backgroundThread.start();
    assertTrue(requestPosted.await(2, TimeUnit.SECONDS), "Request should be posted");
    backgroundThread.join();

    // Cancel the result before processing
    PendingResult<String> result = resultRef.get();
    result.cancel();
    assertTrue(result.isCancelled(), "Result should be cancelled");

    // Try to process the cancelled request
    environment.processRunLaterRequest(requestIdRef.get());

    // Supplier should not have been executed
    assertFalse(supplierExecuted.get(), "Supplier should not execute for cancelled request");
  }
}
