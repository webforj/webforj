package com.webforj.devtools.livereload.receiver;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.devtools.livereload.LiveReloadServer;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.mockito.ArgumentCaptor;

class JrebelReceiverTest {

  private static final String RELOADER_NAME = ReloaderHolder.class.getName();
  private static final String LISTENER_NAME = ReloadListener.class.getName();

  private static final int CLASS_LOADED = 0;
  private static final int CLASS_SWAPPED = 1;

  private Reloader reloader;
  private ClassLoader previousContextClassLoader;

  @BeforeEach
  void setUp() {
    reloader = mock(Reloader.class);
    ReloaderHolder.current = reloader;
    previousContextClassLoader = Thread.currentThread().getContextClassLoader();
  }

  @AfterEach
  void tearDown() {
    Thread.currentThread().setContextClassLoader(previousContextClassLoader);
  }

  @Test
  @Timeout(10)
  void shouldRegisterOnceAndDeregisterOnStop() {
    JrebelReceiver receiver = receiver(runningServer());

    receiver.start();
    receiver.start();

    assertTrue(receiver.isRunning());
    ReloadListener listener = registeredListener();

    receiver.stop();

    assertFalse(receiver.isRunning());
    verify(reloader).removeClassReloadListener(listener);
  }

  @Test
  @Timeout(10)
  void shouldCollapseTheBurstOfSwapsIntoOneClassUpdate() {
    LiveReloadServer server = runningServer();
    CountDownLatch secondUpdate = classUpdateLatch(server, 2, null);

    JrebelReceiver receiver = receiver(server);
    receiver.start();
    ReloadListener listener = registeredListener();

    for (int i = 0; i < 5; i++) {
      listener.onClassEvent(CLASS_SWAPPED, JrebelReceiverTest.class);
    }

    assertFalse(await(secondUpdate, 500));
    assertEquals(1, secondUpdate.getCount());
    verify(server, never()).sendReloadMessage();

    receiver.stop();
  }

  @Test
  @Timeout(10)
  void shouldUpdateForTheSwapButNotForThePlainLoad() {
    LiveReloadServer server = runningServer();
    JrebelReceiver receiver = receiver(server);
    receiver.start();
    ReloadListener listener = registeredListener();

    CountDownLatch load = classUpdateLatch(server, 1, null);
    listener.onClassEvent(CLASS_LOADED, JrebelReceiverTest.class);
    assertFalse(await(load, 400));
    verify(server, never()).sendClassUpdateMessage(any());
    verify(server, never()).sendReloadMessage();

    CountDownLatch swap = classUpdateLatch(server, 1, null);
    listener.onClassEvent(CLASS_SWAPPED, JrebelReceiverTest.class);
    assertTrue(await(swap, 2000));
    verify(server, never()).sendReloadMessage();

    receiver.stop();
  }

  @Test
  @Timeout(10)
  void shouldBatchTheDistinctClassNamesIntoOneUpdate() {
    LiveReloadServer server = runningServer();
    AtomicReference<Set<String>> classes = new AtomicReference<>();
    CountDownLatch update = classUpdateLatch(server, 1, classes);

    JrebelReceiver receiver = receiver(server);
    receiver.start();
    ReloadListener listener = registeredListener();

    listener.onClassEvent(CLASS_SWAPPED, JrebelReceiverTest.class);
    listener.onClassEvent(CLASS_SWAPPED, ReloaderHolder.class);

    assertTrue(await(update, 2000));
    assertEquals(Set.of(JrebelReceiverTest.class.getName(), ReloaderHolder.class.getName()),
        classes.get());
    verify(server, never()).sendReloadMessage();

    receiver.stop();
  }

  @Test
  @Timeout(10)
  void shouldFallBackToTheFullReloadWhenTheClassIsNotNamed() {
    LiveReloadServer server = runningServer();
    CountDownLatch reload = reloadLatch(server, 1);

    JrebelReceiver receiver = receiver(server);
    receiver.start();
    ReloadListener listener = registeredListener();

    listener.onClassEvent(CLASS_SWAPPED, null);

    assertTrue(await(reload, 2000));
    verify(server, never()).sendClassUpdateMessage(any());

    receiver.stop();
  }

  @Test
  @Timeout(10)
  void shouldSendNothingForTheFireWhoseBatchWasAlreadyDrained() {
    LiveReloadServer server = runningServer();
    CountDownLatch update = classUpdateLatch(server, 1, null);

    JrebelReceiver receiver = receiver(server);
    receiver.start();
    registeredListener().onClassEvent(CLASS_SWAPPED, JrebelReceiverTest.class);
    assertTrue(await(update, 2000));

    // A swap racing the drain can leave one scheduled fire behind with nothing left to send.
    receiver.sendReload();

    verify(server).sendClassUpdateMessage(any());
    verify(server, never()).sendReloadMessage();

    receiver.stop();
  }

  @Test
  @Timeout(10)
  void shouldNotReloadThroughTheServerThatIsNotRunning() {
    LiveReloadServer server = mock(LiveReloadServer.class);
    when(server.isRunning()).thenReturn(false);
    CountDownLatch latch = classUpdateLatch(server, 1, null);

    JrebelReceiver receiver = receiver(server);
    receiver.start();
    registeredListener().onClassEvent(CLASS_SWAPPED, JrebelReceiverTest.class);

    assertFalse(await(latch, 400));
    verify(server, never()).sendReloadMessage();
    verify(server, never()).sendClassUpdateMessage(any());

    receiver.stop();
  }

  @Test
  @Timeout(10)
  void shouldIgnoreEventsArrivingAfterStop() {
    LiveReloadServer server = runningServer();
    CountDownLatch latch = classUpdateLatch(server, 1, null);

    JrebelReceiver receiver = receiver(server);
    receiver.start();
    ReloadListener listener = registeredListener();
    receiver.stop();

    assertDoesNotThrow(() -> listener.onClassEvent(CLASS_SWAPPED, JrebelReceiverTest.class));
    assertFalse(await(latch, 400));
  }

  @Test
  @Timeout(10)
  void shouldStayInertWhenTheAgentIsAbsent() {
    JrebelReceiver receiver = receiver(runningServer());

    Thread.currentThread().setContextClassLoader(new URLClassLoader(new URL[0], null));
    receiver.start();

    assertFalse(receiver.isRunning());
    verify(reloader, never()).addClassReloadListener(any());
  }

  @Test
  @Timeout(10)
  void shouldStopHarmlesslyWhenNeverStartedOrWhenDeregistrationFails() {
    assertDoesNotThrow(receiver(runningServer())::stop);

    doThrow(new IllegalStateException("removal is unavailable")).when(reloader)
        .removeClassReloadListener(any());

    JrebelReceiver receiver = receiver(runningServer());
    receiver.start();

    assertDoesNotThrow(receiver::stop);
    assertFalse(receiver.isRunning());
  }

  @Test
  @Timeout(10)
  void shouldLeaveNoThreadBehindWhenTheRegistrationIsRefused() {
    long before = receiverThreadCount();
    doThrow(new IllegalStateException("registration is refused")).when(reloader)
        .addClassReloadListener(any());

    JrebelReceiver receiver = receiver(runningServer());
    receiver.start();

    assertFalse(receiver.isRunning());
    assertEquals(before, receiverThreadCount());
  }

  /**
   * The listener interface the receiver builds its proxy against.
   */
  interface ReloadListener {

    void onClassEvent(int eventType, Class<?> klass);
  }

  /**
   * The registry the receiver registers with, mocked in every test.
   */
  interface Reloader {

    void addClassReloadListener(ReloadListener listener);

    void removeClassReloadListener(ReloadListener listener);
  }

  /**
   * The type the receiver looks up by name to reach the registry.
   */
  static final class ReloaderHolder {

    static Reloader current;

    private ReloaderHolder() {}

    public static Reloader getInstance() {
      return current;
    }
  }

  private ReloadListener registeredListener() {
    ArgumentCaptor<ReloadListener> captor = ArgumentCaptor.forClass(ReloadListener.class);
    verify(reloader).addClassReloadListener(captor.capture());

    return captor.getValue();
  }

  private static long receiverThreadCount() {
    return Thread.getAllStackTraces().keySet().stream()
        .filter(thread -> "webforj-jrebel-receiver".equals(thread.getName()))
        .filter(Thread::isAlive).count();
  }

  private static boolean await(CountDownLatch latch, long millis) {
    try {
      return latch.await(millis, TimeUnit.MILLISECONDS);
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();

      return false;
    }
  }

  private static JrebelReceiver receiver(LiveReloadServer server) {
    return new JrebelReceiver(server, RELOADER_NAME, LISTENER_NAME);
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
