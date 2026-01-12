package com.webforj;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import com.webforj.component.element.Element;
import java.util.concurrent.atomic.AtomicBoolean;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class ViewTransitionTest {

  private Page mockPage;
  private ViewTransitionManager mockManager;
  private ViewTransition viewTransition;

  @BeforeEach
  void setUp() {
    mockPage = mock(Page.class);
    mockManager = mock(ViewTransitionManager.class);
    viewTransition = new ViewTransition(mockPage, mockManager);
  }

  @Nested
  @DisplayName("Initialization")
  class Initialization {

    @Test
    @DisplayName("should generate unique transition ID")
    void shouldGenerateTransitionId() {
      assertNotNull(viewTransition.getTransitionId());
      assertFalse(viewTransition.getTransitionId().isEmpty());
    }
  }

  @Nested
  @DisplayName("Builder API")
  class BuilderApi {

    @Test
    @DisplayName("should store default type")
    void shouldStoreType() {
      viewTransition.type("fade");
      assertEquals("fade", viewTransition.getDefaultType());
    }

    @Test
    @DisplayName("should return same instance for exit and enter chaining")
    void shouldReturnSameInstanceForExitAndEnter() {
      Element mockComponent = mock(Element.class);
      assertSame(viewTransition, viewTransition.exit(mockComponent));
      assertSame(viewTransition, viewTransition.exit(mockComponent, ViewTransition.FADE));
      assertSame(viewTransition, viewTransition.enter(mockComponent));
      assertSame(viewTransition, viewTransition.enter(mockComponent, ViewTransition.SLIDE_LEFT));
    }
  }

  @Nested
  @DisplayName("Starting Transition")
  class StartingTransition {

    @Test
    @DisplayName("should throw when starting without update callback")
    void shouldThrowOnStartWithoutUpdateCallback() {
      IllegalStateException ex =
          assertThrows(IllegalStateException.class, () -> viewTransition.start());
      assertTrue(ex.getMessage().contains("onUpdate"));
    }

    @Test
    @DisplayName("should register transition and execute JS on start")
    void shouldRegisterAndExecuteJsOnStart() {
      viewTransition.onUpdate(done -> done.run());
      viewTransition.start();
      verify(mockManager).register(eq(viewTransition.getTransitionId()), eq(viewTransition));
      verify(mockPage).executeJsVoidAsync(anyString());
    }
  }

  @Nested
  @DisplayName("Callback Execution")
  class CallbackExecution {

    @Test
    @DisplayName("should execute update callback with done handler")
    void shouldRunUpdateCallback() {
      AtomicBoolean called = new AtomicBoolean(false);
      viewTransition.onUpdate(done -> {
        called.set(true);
        done.run();
      });
      viewTransition.executeUpdate(() -> {
      });
      assertTrue(called.get());
    }

    @Test
    @DisplayName("should execute ready callback")
    void shouldRunReadyCallback() {
      AtomicBoolean called = new AtomicBoolean(false);
      viewTransition.onReady(() -> called.set(true));
      viewTransition.executeReady();
      assertTrue(called.get());
    }

    @Test
    @DisplayName("should handle null callbacks gracefully")
    void shouldHandleNullCallbacks() {
      assertDoesNotThrow(() -> viewTransition.executeUpdate(() -> {
      }));
      assertDoesNotThrow(() -> viewTransition.executeReady());
    }
  }

  @Nested
  @DisplayName("Exit Styles")
  class ExitStyles {

    @Test
    @DisplayName("should apply exit styles to registered components")
    void shouldApplyExitStyles() {
      Element mockComponent = mock(Element.class);
      viewTransition.exit(mockComponent, ViewTransition.FADE);
      assertDoesNotThrow(() -> viewTransition.applyExitStyles());
    }

    @Test
    @DisplayName("should skip NONE transition type")
    void shouldHandleNoneTransitionType() {
      Element mockComponent = mock(Element.class);
      viewTransition.exit(mockComponent, ViewTransition.NONE);
      assertDoesNotThrow(() -> viewTransition.applyExitStyles());
    }

    @Test
    @DisplayName("should handle null component gracefully")
    void shouldHandleNullComponent() {
      assertDoesNotThrow(() -> viewTransition.exit(null, ViewTransition.FADE));
      assertDoesNotThrow(() -> viewTransition.applyExitStyles());
    }
  }
}
