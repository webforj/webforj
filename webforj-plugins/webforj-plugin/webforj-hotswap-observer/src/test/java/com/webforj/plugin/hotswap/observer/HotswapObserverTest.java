package com.webforj.plugin.hotswap.observer;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import com.webforj.devtools.livereload.receiver.HotswapAgentReceiver;
import java.lang.instrument.Instrumentation;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HotswapObserverTest {

  private static final String CLASS_NAME = "com/example/DashboardView";

  private HotswapObserver observer;

  @BeforeEach
  void setUp() {
    HotswapAgentReceiver.reset();
    observer = new HotswapObserver();
  }

  @Test
  void shouldRegisterItselfInTheVirtualMachine() {
    Instrumentation instrumentation = mock(Instrumentation.class);

    HotswapObserver.premain(null, instrumentation);

    verify(instrumentation).addTransformer(any(HotswapObserver.class));
  }

  @Test
  void shouldIgnoreTheFirstLoadOfTheClass() {
    assertNull(observer.transform(getClass().getModule(), getClass().getClassLoader(), CLASS_NAME,
        null, null, new byte[] {1}));

    assertTrue(HotswapAgentReceiver.reports().isEmpty());
  }

  @Test
  void shouldReportTheRedefinitionWithTheBinaryName() throws Exception {
    byte[] observed = {1, 2, 3};

    byte[] result = observer.transform(getClass().getModule(), getClass().getClassLoader(),
        CLASS_NAME, String.class, null, observed);

    assertNull(result, "the observer never changes the bytes");
    assertTrue(HotswapAgentReceiver.awaitReport(5, TimeUnit.SECONDS),
        "the redefinition reaches the receiver");
    assertEquals("com.example.DashboardView", HotswapAgentReceiver.reports().get(0)[0],
        "the internal name arrives as the binary name");
    assertArrayEquals(observed, (byte[]) HotswapAgentReceiver.reports().get(0)[1]);
  }
}
