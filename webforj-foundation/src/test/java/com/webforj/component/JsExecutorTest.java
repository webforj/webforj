package com.webforj.component;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import com.basis.startup.type.BBjException;
import com.webforj.Page;
import com.webforj.PendingResult;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class JsExecutorTest {
  JsExecutorMock jsExecutor;
  Component component;

  @BeforeEach
  void setUp() throws BBjException {
    component = mock(Component.class);
    jsExecutor = spy(new JsExecutorMock(component));
  }

  @Test
  @DisplayName("Test executeJs on attached component")
  void testExecuteJsWhenAttached() {
    when(component.isAttached()).thenReturn(true);
    String script = "console.log('script')";
    Object result = jsExecutor.executeJs(script);

    assertNotNull(result);
    assertTrue(jsExecutor.isExecuted(script));
  }

  @Test
  @DisplayName("Test executeJs on detached component and then attaching")
  void testExecuteJsWhenNotAttached() {
    String script = "console.log('script')";
    String expectedResult = "Executed: " + script;

    Page page = mock(Page.class);
    MockedStatic<Page> mockedPage = mockStatic(Page.class);
    mockedPage.when(Page::getCurrent).thenReturn(page);
    when(jsExecutor.getPage()).thenReturn(page);
    when(page.executeJs(script)).thenReturn(expectedResult);

    when(component.isAttached()).thenReturn(false);
    when(jsExecutor.getPage()).thenReturn(page);

    Object result = jsExecutor.executeJs(script);

    assertNotNull(result);
    assertEquals(expectedResult, result);

    mockedPage.close();
  }

  @Test
  @DisplayName("Test executeJsAsync on attached component")
  void testExecuteJsAsyncWhenAttached() {
    when(component.isAttached()).thenReturn(true);
    String script = "console.log('script')";
    PendingResult<Object> operation = jsExecutor.executeJsAsync(script);

    assertNotNull(operation);
    assertFalse(operation.isDone());

    AtomicReference<String> resultRef = new AtomicReference<>();
    operation.complete("Executed: " + script);
    operation.thenAccept(result -> resultRef.set((String) result));

    jsExecutor.executeQueuedScripts();

    assertEquals("Executed: " + script, resultRef.get());
    assertTrue(operation.isDone());
  }

  @Test
  @DisplayName("Test cancellation of executeJsAsync")
  void testExecuteJsAsyncCancellation() {
    String script = "console.log('script')";
    PendingResult<Object> operation = jsExecutor.executeJsAsync(script);

    assertNotNull(operation);
    assertFalse(operation.isDone());

    operation.cancel();

    assertTrue(operation.isCancelled());
    assertTrue(operation.isDone());
  }
}
