package com.webforj;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;

import com.webforj.utilities.Assets;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class ViewTransitionManagerTest {

  @Test
  void shouldCreateManagerAndInjectAssets() {
    try (MockedStatic<Assets> assets = mockStatic(Assets.class)) {
      assets.when(() -> Assets.contentOf(anyString())).thenReturn("content");
      Page page = mock(Page.class);

      assertDoesNotThrow(() -> new ViewTransitionManager(page));

      verify(page).addInlineStyleSheet(anyString());
      verify(page).executeJsVoidAsync(anyString());
      verify(page).addEventListener(eq("vt-event"), any(), any());
    }
  }

  @Test
  void shouldExecutePrepareAndResolve() {
    try (MockedStatic<Assets> assets = mockStatic(Assets.class)) {
      assets.when(() -> Assets.contentOf(anyString())).thenReturn("");
      Page page = mock(Page.class);
      ViewTransitionManager manager = new ViewTransitionManager(page);
      ViewTransition transition = mock(ViewTransition.class);

      manager.register("test-id", transition);
      manager.executePrepare("test-id");

      verify(transition).applyExitStyles();
      verify(page).executeJsVoidAsync(contains("resolvePrepare"));
    }
  }

  @Test
  void shouldExecuteUpdateCallbackAndResolvePromise() {
    try (MockedStatic<Assets> assets = mockStatic(Assets.class)) {
      assets.when(() -> Assets.contentOf(anyString())).thenReturn("");
      Page page = mock(Page.class);
      ViewTransitionManager manager = new ViewTransitionManager(page);
      ViewTransition transition = mock(ViewTransition.class);

      doAnswer(invocation -> {
        Runnable onComplete = invocation.getArgument(0);
        onComplete.run();
        return null;
      }).when(transition).executeUpdate(any(Runnable.class));

      manager.register("test-id", transition);
      manager.executeUpdate("test-id");

      verify(transition).executeUpdate(any(Runnable.class));
      verify(page).executeJsVoidAsync(contains("resolveUpdate"));
    }
  }

  @Test
  void shouldExecuteReadyCallback() {
    try (MockedStatic<Assets> assets = mockStatic(Assets.class)) {
      assets.when(() -> Assets.contentOf(anyString())).thenReturn("");
      Page page = mock(Page.class);
      ViewTransitionManager manager = new ViewTransitionManager(page);
      ViewTransition transition = mock(ViewTransition.class);

      manager.register("test-id", transition);
      manager.executeReady("test-id");

      verify(transition).executeReady();
    }
  }

  @Test
  void shouldCompleteTransitionAndRemoveFromPending() {
    try (MockedStatic<Assets> assets = mockStatic(Assets.class)) {
      assets.when(() -> Assets.contentOf(anyString())).thenReturn("");
      Page page = mock(Page.class);
      ViewTransitionManager manager = new ViewTransitionManager(page);
      ViewTransition transition = mock(ViewTransition.class);

      manager.register("test-id", transition);
      assertDoesNotThrow(() -> manager.executeComplete("test-id"));
    }
  }

  @Test
  void shouldHandleUnknownTransitionId() {
    try (MockedStatic<Assets> assets = mockStatic(Assets.class)) {
      assets.when(() -> Assets.contentOf(anyString())).thenReturn("");
      Page page = mock(Page.class);
      ViewTransitionManager manager = new ViewTransitionManager(page);

      assertDoesNotThrow(() -> manager.executePrepare("unknown-id"));
      assertDoesNotThrow(() -> manager.executeUpdate("unknown-id"));
      assertDoesNotThrow(() -> manager.executeReady("unknown-id"));
      assertDoesNotThrow(() -> manager.executeComplete("unknown-id"));
    }
  }
}
