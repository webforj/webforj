package org.dwcj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.BBjWebManager;
import com.basis.startup.type.BBjException;
import java.util.concurrent.atomic.AtomicReference;
import org.dwcj.bridge.IDwcjBBjBridge;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class PageTest {
  Page page;
  Environment environment;
  BBjAPI api;
  BBjWebManager webManager;
  BBjSysGui sysGui;
  IDwcjBBjBridge bridge;

  @BeforeEach
  void setUp() throws BBjException {
    environment = mock(Environment.class);
    api = mock(BBjAPI.class);
    webManager = mock(BBjWebManager.class);
    sysGui = mock(BBjSysGui.class);
    bridge = mock(IDwcjBBjBridge.class);

    when(environment.getBBjAPI()).thenReturn(api);
    when(environment.getSysGui()).thenReturn(sysGui);
    when(environment.getDwcjHelper()).thenReturn(bridge);
    when(api.getWebManager()).thenReturn(webManager);

    page = spy(Page.class);
    when(page.getEnvironment()).thenReturn(environment);
  }

  @Test
  void testExecuteJs() throws BBjException {
    String js = "console.log('script')";
    page.executeJs(js);
    verify(webManager).executeScript(js);
  }

  @Test
  void testExecuteJsAsync() {
    String script = "console.log('script')";
    PendingResult<Object> operation = page.executeJsAsync(script);

    assertNotNull(operation);
    assertFalse(operation.isDone());

    AtomicReference<String> resultRef = new AtomicReference<>();
    operation.complete("Executed: " + script);
    operation.thenAccept(result -> resultRef.set((String) result));

    assertEquals("Executed: " + script, resultRef.get());
    assertTrue(operation.isDone());
  }

  @Test
  void testReload() throws BBjException {
    page.reload();
    verify(page, times(1)).executeJsAsync("window.location.reload();");
  }
}
