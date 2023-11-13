package org.dwcj;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjExecuteScriptEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.CustomObject;
import java.util.HashMap;
import java.util.Map;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * Handler for the Page {@code BBjExecuteScriptEvent} event.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public final class PageExecuteJsAsyncHandler {
  private final Map<Integer, PendingResult<Object>> pendingResults = new HashMap<>();
  private final Environment env;
  private boolean registered = false;

  PageExecuteJsAsyncHandler(Environment environment) {
    env = environment;
  }

  /**
   * Register the handler to handle the Page ExecuteJsAsync event in case it is not registered yet.
   */
  void register() {
    if (registered) {
      return;
    }

    CustomObject handler = env.getDwcjHelper().getEventProxy(this, "handleEvent");
    try {
      env.getBBjAPI().getWebManager().setCallback(SysGuiEventConstants.ON_EXECUTE_SCRIPT, handler,
          "onEvent");
      registered = true;
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to set Page ExecuteJsAsyncHandler.", e);
    }
  }

  /**
   * Handle the Page ExecuteJsAsync event.
   *
   * @param ev The event
   */
  public void handleEvent(BBjEvent ev) {
    BBjExecuteScriptEvent event = (BBjExecuteScriptEvent) ev;

    try {
      int index = Integer.parseInt(event.getIndex() + "");
      Object result = event.getResult();
      PendingResult<Object> pending = pendingResults.remove(index);

      if (pending != null) {
        pending.complete(result);
      }
    } catch (BBjException e) {
      throw new DwcjRuntimeException(e);
    }
  }

  /**
   * Get the map of pending results.
   *
   * @return The map of pending results
   */
  public Map<Integer, PendingResult<Object>> getPendingResults() {
    return pendingResults;
  }
}
