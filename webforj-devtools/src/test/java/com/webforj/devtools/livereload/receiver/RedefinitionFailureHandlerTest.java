package com.webforj.devtools.livereload.receiver;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class RedefinitionFailureHandlerTest {

  private Thread.UncaughtExceptionHandler originalDefault;

  @BeforeEach
  void rememberDefaultHandler() {
    originalDefault = Thread.getDefaultUncaughtExceptionHandler();
  }

  @AfterEach
  void restoreDefaultHandler() {
    Thread.setDefaultUncaughtExceptionHandler(originalDefault);
  }

  @Test
  void shouldHandTheRejectionReasonFromTheNestedCauseToTheConsumer() {
    AtomicReference<String> reason = new AtomicReference<>();
    RedefinitionFailureHandler handler = new RedefinitionFailureHandler(reason::set);

    handler.uncaughtException(new Thread("hotswap"), rejection("attempted to change the schema"));

    assertEquals("attempted to change the schema", reason.get());
  }

  @Test
  void shouldIgnoreTheExceptionThatIsNotTheRejection() {
    AtomicReference<String> reason = new AtomicReference<>();
    RedefinitionFailureHandler handler = new RedefinitionFailureHandler(reason::set);

    handler.uncaughtException(new Thread("worker"),
        new IllegalStateException("something unrelated"));

    assertNull(reason.get());
  }

  @Test
  void shouldIgnoreTheSameExceptionTypeRaisedOutsideTheRedefinition() {
    AtomicReference<String> reason = new AtomicReference<>();
    RedefinitionFailureHandler handler = new RedefinitionFailureHandler(reason::set);

    handler.uncaughtException(new Thread("worker"),
        new UnsupportedOperationException("not from the instrumentation"));

    assertNull(reason.get());
  }

  @Test
  void shouldForwardEveryExceptionToTheHandlerInstalledBefore() {
    List<Throwable> forwarded = new ArrayList<>();
    Thread.setDefaultUncaughtExceptionHandler((thread, exception) -> forwarded.add(exception));

    RedefinitionFailureHandler handler = new RedefinitionFailureHandler(reason -> {
    });
    handler.install();
    try {
      Throwable recognized = rejection("attempted to change the schema");
      Throwable unrelated = new IllegalStateException("something unrelated");
      Thread.getDefaultUncaughtExceptionHandler().uncaughtException(new Thread("t"), recognized);
      Thread.getDefaultUncaughtExceptionHandler().uncaughtException(new Thread("t"), unrelated);

      assertEquals(List.of(recognized, unrelated), forwarded);
    } finally {
      handler.uninstall();
    }
  }

  @Test
  void shouldPrintTheDefaultFormWhenNoHandlerWasInstalledBefore() {
    ByteArrayOutputStream captured = new ByteArrayOutputStream();
    PrintStream originalErr = System.err;
    System.setErr(new PrintStream(captured, true, StandardCharsets.UTF_8));
    try {
      RedefinitionFailureHandler handler = new RedefinitionFailureHandler(reason -> {
      });

      handler.uncaughtException(new Thread("hotswap"), rejection("attempted to change"));
    } finally {
      System.setErr(originalErr);
    }

    String printed = captured.toString(StandardCharsets.UTF_8);
    assertTrue(printed.startsWith("Exception in thread \"hotswap\" "));
    assertTrue(printed.contains("Unable to redefine classes"));
  }

  @Test
  void shouldSurviveTheConsumerThatThrows() {
    List<Throwable> forwarded = new ArrayList<>();
    Thread.setDefaultUncaughtExceptionHandler((thread, exception) -> forwarded.add(exception));

    RedefinitionFailureHandler handler = new RedefinitionFailureHandler(reason -> {
      throw new IllegalStateException("consumer failure");
    });
    handler.install();
    try {
      Throwable recognized = rejection("attempted to change the schema");
      Thread.getDefaultUncaughtExceptionHandler().uncaughtException(new Thread("t"), recognized);

      assertEquals(List.of(recognized), forwarded);
    } finally {
      handler.uninstall();
    }
  }

  @Test
  void shouldRestoreTheHandlerInstalledBeforeOnUninstall() {
    Thread.UncaughtExceptionHandler before = (thread, exception) -> {
    };
    Thread.setDefaultUncaughtExceptionHandler(before);

    RedefinitionFailureHandler handler = new RedefinitionFailureHandler(reason -> {
    });
    handler.install();
    handler.uninstall();

    assertSame(before, Thread.getDefaultUncaughtExceptionHandler());
  }

  @Test
  void shouldLeaveTheReplacementUntouchedOnUninstall() {
    RedefinitionFailureHandler handler = new RedefinitionFailureHandler(reason -> {
    });
    handler.install();

    Thread.UncaughtExceptionHandler replacement = (thread, exception) -> {
    };
    Thread.setDefaultUncaughtExceptionHandler(replacement);
    handler.uninstall();

    assertSame(replacement, Thread.getDefaultUncaughtExceptionHandler());
  }

  private static Throwable rejection(String reason) {
    UnsupportedOperationException cause = new UnsupportedOperationException(reason);
    cause.setStackTrace(new StackTraceElement[] {
        new StackTraceElement("sun.instrument.InstrumentationImpl", "redefineClasses0", null, -2)});

    IllegalStateException wrapper = new IllegalStateException("Unable to redefine classes", cause);
    wrapper.setStackTrace(
        new StackTraceElement[] {new StackTraceElement("org.hotswap.agent.config.PluginManager",
            "hotswap", "PluginManager.java", 304)});

    return wrapper;
  }
}
