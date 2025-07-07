package com.webforj;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjSysGui;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.WebforjBBjBridge;
import java.util.concurrent.atomic.AtomicBoolean;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class EnvironmentRunLaterTest {

  @Mock
  private BBjAPI mockApi;

  @Mock
  private WebforjBBjBridge mockBridge;

  @Mock
  private BBjSysGui mockSysGui;

  @BeforeEach
  void setUp() throws BBjException {
    when(mockApi.openSysGui(anyString())).thenReturn(mockSysGui);
    Environment.init(mockApi, mockBridge, 0);
  }

  @AfterEach
  void tearDown() {
    try {
      Environment.cleanup();
    } catch (Exception e) {
      // Ignore cleanup errors
    }
  }

  @Test
  void shouldExecuteImmediatelyFromMainThread() {
    AtomicBoolean executed = new AtomicBoolean(false);

    // Call from main thread
    PendingResult<String> result = Environment.runLater(() -> {
      executed.set(true);
      return "test";
    });

    // Should execute immediately
    assertTrue(executed.get(), "Should execute immediately in main thread");
    assertNotNull(result, "Should return PendingResult");
    assertTrue(result.isDone(), "Result should be immediately done");
  }

  @Test
  void shouldQueueTaskFromBackgroundThread() throws Exception {
    AtomicBoolean queued = new AtomicBoolean(false);

    // Create a thread that inherits the Environment context
    Thread backgroundThread = new Thread(() -> {
      Environment.runLater(() -> {
        return "background";
      });
      queued.set(true);
    });

    // Start and wait for the thread
    backgroundThread.start();
    // Wait up to 1 second
    backgroundThread.join(1000);

    assertTrue(queued.get(), "Task should be queued");
  }

  @Test
  void shouldAcceptRunnable() {
    AtomicBoolean executed = new AtomicBoolean(false);

    PendingResult<Void> result = Environment.runLater(() -> {
      executed.set(true);
    });

    assertTrue(executed.get(), "Runnable should execute");
    assertNotNull(result, "Should return PendingResult<Void>");
    assertTrue(result.isDone(), "Should be done");
  }
}
