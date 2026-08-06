package com.webforj.devtools.livereload.receiver;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.devtools.livereload.LiveReloadServer;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

class HotswapAgentReceiverTest {

  private static final String CLASS_NAME = HotswapAgentReceiverTest.class.getName();
  private static final String OTHER_CLASS_NAME = HotswapAgentReceiverFixture.class.getName();

  private HotswapAgentReceiver receiver;

  @AfterEach
  void tearDown() {
    if (receiver != null) {
      receiver.stop();
    }
  }

  @Test
  @Timeout(10)
  void shouldRegisterOnceAndDeregisterOnStop() {
    receiver = HotswapAgentReceiverFixture.withAgentDetected(runningServer());

    receiver.start();
    receiver.start();
    assertTrue(receiver.isRunning());

    receiver.stop();
    assertFalse(receiver.isRunning());
  }

  @Test
  @Timeout(10)
  void shouldStayInertWithoutTheAgentArgument() {
    receiver = new HotswapAgentReceiver(runningServer(), List.of("-Xmx1g"));

    receiver.start();

    assertFalse(receiver.isRunning());
  }

  @Test
  @Timeout(10)
  void shouldCollapseTheBurstOfRedefinitionsIntoOneClassUpdate() {
    LiveReloadServer server = runningServer();
    AtomicReference<Set<String>> classes = new AtomicReference<>();
    final CountDownLatch update = classUpdateLatch(server, 1, classes);

    receiver = HotswapAgentReceiverFixture.withAgentDetected(server);
    receiver.start();

    for (int i = 0; i < 5; i++) {
      HotswapAgentReceiver.onClassRedefinition(CLASS_NAME, new byte[] {(byte) i});
    }

    assertTrue(await(update, 2000));
    assertEquals(Set.of(CLASS_NAME), classes.get());
    verify(server, never()).sendReloadMessage();
  }

  @Test
  @Timeout(10)
  void shouldBatchTheDistinctClassNamesIntoOneUpdate() {
    LiveReloadServer server = runningServer();
    AtomicReference<Set<String>> classes = new AtomicReference<>();
    final CountDownLatch update = classUpdateLatch(server, 1, classes);

    receiver = HotswapAgentReceiverFixture.withAgentDetected(server);
    receiver.start();

    HotswapAgentReceiver.onClassRedefinition(CLASS_NAME, new byte[] {1});
    HotswapAgentReceiver.onClassRedefinition(OTHER_CLASS_NAME, new byte[] {2});

    assertTrue(await(update, 2000));
    assertEquals(Set.of(CLASS_NAME, OTHER_CLASS_NAME), classes.get());
    verify(server, never()).sendReloadMessage();
  }

  @Test
  @Timeout(10)
  void shouldTranslateTheInternalNameIntoTheBinaryName() {
    LiveReloadServer server = runningServer();
    AtomicReference<Set<String>> classes = new AtomicReference<>();
    final CountDownLatch update = classUpdateLatch(server, 1, classes);

    receiver = HotswapAgentReceiverFixture.withAgentDetected(server);
    receiver.start();

    HotswapAgentReceiver.onClassRedefinition("com/hotswapspring/views/DashboardView",
        new byte[] {1});

    assertTrue(await(update, 2000));
    assertEquals(Set.of("com.hotswapspring.views.DashboardView"), classes.get());
  }

  @Test
  @Timeout(10)
  void shouldSkipTheRedefinitionThatCarriesUnchangedBytes() {
    LiveReloadServer server = runningServer();
    final CountDownLatch first = classUpdateLatch(server, 1, null);

    receiver = HotswapAgentReceiverFixture.withAgentDetected(server);
    receiver.start();

    byte[] bytes = new byte[] {1, 2, 3};
    HotswapAgentReceiver.onClassRedefinition(CLASS_NAME, bytes);
    assertTrue(await(first, 2000));

    CountDownLatch second = classUpdateLatch(server, 1, null);
    HotswapAgentReceiver.onClassRedefinition(CLASS_NAME, bytes);
    assertFalse(await(second, 400));

    HotswapAgentReceiver.onClassRedefinition(CLASS_NAME, new byte[] {4, 5, 6});
    assertTrue(await(second, 2000));
  }

  @Test
  @Timeout(10)
  void shouldFallBackToTheFullReloadWhenTheClassIsNotNamed() {
    LiveReloadServer server = runningServer();
    final CountDownLatch reload = reloadLatch(server, 1);

    receiver = HotswapAgentReceiverFixture.withAgentDetected(server);
    receiver.start();

    HotswapAgentReceiver.onClassRedefinition(null, new byte[] {1});

    assertTrue(await(reload, 2000));
    verify(server, never()).sendClassUpdateMessage(any());
  }

  @Test
  @Timeout(10)
  void shouldSendNothingForTheFireWhoseBatchWasAlreadyDrained() {
    LiveReloadServer server = runningServer();
    final CountDownLatch update = classUpdateLatch(server, 1, null);

    receiver = HotswapAgentReceiverFixture.withAgentDetected(server);
    receiver.start();
    HotswapAgentReceiver.onClassRedefinition(CLASS_NAME, new byte[] {1});
    assertTrue(await(update, 2000));

    // A redefinition racing the drain can leave one scheduled fire behind with nothing to send.
    receiver.sendReload();

    verify(server).sendClassUpdateMessage(any());
    verify(server, never()).sendReloadMessage();
  }

  @Test
  @Timeout(10)
  void shouldNotUpdateThroughTheServerThatIsNotRunning() {
    LiveReloadServer server = mock(LiveReloadServer.class);
    when(server.isRunning()).thenReturn(false);
    final CountDownLatch update = classUpdateLatch(server, 1, null);

    receiver = HotswapAgentReceiverFixture.withAgentDetected(server);
    receiver.start();
    HotswapAgentReceiver.onClassRedefinition(CLASS_NAME, new byte[] {1});

    assertFalse(await(update, 400));
    verify(server, never()).sendReloadMessage();
    verify(server, never()).sendClassUpdateMessage(any());
  }

  @Test
  @Timeout(10)
  void shouldIgnoreEventsArrivingAfterStop() {
    LiveReloadServer server = runningServer();
    final CountDownLatch update = classUpdateLatch(server, 1, null);

    receiver = HotswapAgentReceiverFixture.withAgentDetected(server);
    receiver.start();
    receiver.stop();

    assertDoesNotThrow(() -> HotswapAgentReceiver.onClassRedefinition(CLASS_NAME, new byte[] {1}));
    assertFalse(await(update, 400));
  }

  @Test
  @Timeout(10)
  void shouldReportTheRejectionInsteadOfTheUpdate() {
    LiveReloadServer server = runningServer();
    AtomicReference<Set<String>> classes = new AtomicReference<>();
    AtomicReference<String> reason = new AtomicReference<>();
    final CountDownLatch rejection = rejectionLatch(server, 1, classes, reason);

    Thread.UncaughtExceptionHandler original = Thread.getDefaultUncaughtExceptionHandler();
    Thread.setDefaultUncaughtExceptionHandler((thread, exception) -> {
    });
    try {
      receiver = HotswapAgentReceiverFixture.withAgentDetected(server);
      receiver.start();

      HotswapAgentReceiver.onClassRedefinition(CLASS_NAME, new byte[] {1});
      Thread.getDefaultUncaughtExceptionHandler().uncaughtException(new Thread("hotswap"),
          redefinitionRejection("attempted to change the schema (add/remove fields)"));

      assertTrue(await(rejection, 2000));
      assertEquals(Set.of(CLASS_NAME), classes.get());
      assertEquals("attempted to change the schema (add/remove fields)", reason.get());
      verify(server, never()).sendClassUpdateMessage(any());
      verify(server, never()).sendReloadMessage();
    } finally {
      receiver.stop();
      Thread.setDefaultUncaughtExceptionHandler(original);
    }
  }

  @Test
  @Timeout(10)
  void shouldForgetTheDigestsOfTheRejectedBytes() {
    LiveReloadServer server = runningServer();
    final CountDownLatch rejection = rejectionLatch(server, 1, null, null);

    Thread.UncaughtExceptionHandler original = Thread.getDefaultUncaughtExceptionHandler();
    Thread.setDefaultUncaughtExceptionHandler((thread, exception) -> {
    });
    try {
      receiver = HotswapAgentReceiverFixture.withAgentDetected(server);
      receiver.start();

      byte[] bytes = new byte[] {1, 2, 3};
      HotswapAgentReceiver.onClassRedefinition(CLASS_NAME, bytes);
      Thread.getDefaultUncaughtExceptionHandler().uncaughtException(new Thread("hotswap"),
          redefinitionRejection("rejected"));
      assertTrue(await(rejection, 2000));

      // The same bytes come back when the developer retries the same edit. The rejected attempt
      // never became the running code, so the retry must report as a fresh change.
      CountDownLatch update = classUpdateLatch(server, 1, null);
      HotswapAgentReceiver.onClassRedefinition(CLASS_NAME, bytes);
      assertTrue(await(update, 2000));
    } finally {
      receiver.stop();
      Thread.setDefaultUncaughtExceptionHandler(original);
    }
  }

  private static CountDownLatch rejectionLatch(LiveReloadServer server, int count,
      AtomicReference<Set<String>> lastClasses, AtomicReference<String> lastReason) {
    CountDownLatch latch = new CountDownLatch(count);
    doAnswer(invocation -> {
      if (lastClasses != null) {
        lastClasses.set(invocation.getArgument(0));
      }
      if (lastReason != null) {
        lastReason.set(invocation.getArgument(1));
      }
      latch.countDown();
      return null;
    }).when(server).sendClassUpdateErrorMessage(any(), any());

    return latch;
  }

  private static Throwable redefinitionRejection(String reason) {
    UnsupportedOperationException cause = new UnsupportedOperationException(reason);
    cause.setStackTrace(new StackTraceElement[] {
        new StackTraceElement("sun.instrument.InstrumentationImpl", "redefineClasses0", null, -2)});

    return new IllegalStateException("Unable to redefine classes", cause);
  }

  private static boolean await(CountDownLatch latch, long millis) {
    try {
      return latch.await(millis, TimeUnit.MILLISECONDS);
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();

      return false;
    }
  }

  private static CountDownLatch reloadLatch(LiveReloadServer server, int count) {
    CountDownLatch latch = new CountDownLatch(count);
    doAnswer(invocation -> {
      latch.countDown();
      return null;
    }).when(server).sendReloadMessage();

    return latch;
  }

  private static CountDownLatch classUpdateLatch(LiveReloadServer server, int count,
      AtomicReference<Set<String>> lastClasses) {
    CountDownLatch latch = new CountDownLatch(count);
    doAnswer(invocation -> {
      if (lastClasses != null) {
        lastClasses.set(invocation.getArgument(0));
      }

      latch.countDown();
      return null;
    }).when(server).sendClassUpdateMessage(any());

    return latch;
  }

  private static LiveReloadServer runningServer() {
    LiveReloadServer server = mock(LiveReloadServer.class);
    when(server.isRunning()).thenReturn(true);

    return server;
  }
}
