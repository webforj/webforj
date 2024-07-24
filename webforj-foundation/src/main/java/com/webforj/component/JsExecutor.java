package com.webforj.component;

import com.basis.startup.type.BBjException;
import com.webforj.Page;
import com.webforj.PendingResult;
import com.webforj.concern.HasJsExecution;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

/**
 * Abstract class for executing JavaScript scripts in a component context.
 *
 * <p>
 * This class provides a foundation for executing JavaScript synchronously or asynchronously within
 * a component. It includes a queuing mechanism for scripts when a component is not yet ready for
 * script execution, e.g., not attached to the DOM.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public abstract class JsExecutor implements HasJsExecution {

  private final Component component;
  private final List<Entry<String, PendingResult<Object>>> scriptsStore = new ArrayList<>();
  private final Map<Integer, PendingResult<Object>> resultsStore = new HashMap<>();

  /**
   * Constructs a new JsExecutor associated with a given component.
   *
   * @param component The component context within which JavaScript will be executed.
   */
  protected JsExecutor(Component component) {
    this.component = component;
  }

  /**
   * Retrieves the PendingResult associated with a specific asynchronous script execution.
   *
   * @param id The identifier of the asynchronous script execution.
   * @return The PendingResult associated with the given id, or null if not found.
   */
  public PendingResult<Object> getPendingResultByIndex(int id) {
    return resultsStore.remove(id);
  }

  /**
   * Executes all queued scripts. This method should be called when the component is ready to
   * process the queued scripts. Typically this is done when the component is attached to the DOM.
   */
  public void executeQueuedScripts() {
    List<Entry<String, PendingResult<Object>>> scriptsStoreCopy =
        new ArrayList<>(this.scriptsStore);

    if (!scriptsStoreCopy.isEmpty()) {
      scriptsStoreCopy.forEach(entry -> {
        String script = entry.getKey();
        PendingResult<Object> result = entry.getValue();
        executeJsAsync(script, result, result != null);
      });
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Object executeJs(String script) {
    if (component.isAttached()) {
      try {
        return doExecuteJs(script);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    } else {
      // we fallback to Page to execute the script.
      // this is needed for example when the component is not attached to the DOM
      // The component variable will be null in this case.
      return getPage().executeJs(script);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public PendingResult<Object> executeJsAsync(String script) {
    return executeJsAsync(script, null, true);
  }

  /**
   * Executes a provided JavaScript script asynchronously and returns a PendingResult if possible.
   *
   * @param script The JavaScript code to execute asynchronously.
   * @param pending A {@link PendingResult} that will be completed when the script execution is
   *        finished.
   * @param returnResult A boolean value indicating whether the script result should be returned to
   *        the server in the next event cycle.
   *
   * @return An {@link PendingResult} representing the pending result of the script execution.
   */
  private PendingResult<Object> executeJsAsync(String script, PendingResult<Object> pending,
      boolean returnResult) {
    PendingResult<Object> result = pending;
    if (result == null) {
      result = new PendingResult<>();
    }

    if (component.isAttached()) {
      try {
        if (!result.isCancelled()) {
          int id = returnResult ? doExecuteJsAsync(script) : doExecuteJsVoidAsync(script);
          resultsStore.put(id, returnResult ? result : null);
        }
      } catch (BBjException e) {
        if (pending != null) {
          pending.completeExceptionally(e);
        }
      }
    } else {
      scriptsStore.add(new SimpleEntry<>(script, returnResult ? result : null));
    }

    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void executeJsVoidAsync(String script) {
    executeJsAsync(script, null, false);
  }

  /**
   * Retrieves the current page instance.
   *
   * @return the current page instance
   */
  public Page getPage() {
    return Page.getCurrent();
  }

  /**
   * Abstract method to execute JavaScript synchronously.
   *
   * @param script The JavaScript code to execute.
   * @return The result of the script execution.
   *
   * @throws BBjException if an error occurs during script execution.
   */
  protected abstract Object doExecuteJs(String script) throws BBjException;

  /**
   * Abstract method to execute JavaScript asynchronously.
   *
   * @param script The JavaScript code to execute asynchronously.
   * @return An integer identifier for the asynchronous execution.
   *
   * @throws BBjException if an error occurs during script execution.
   */
  protected abstract int doExecuteJsAsync(String script) throws BBjException;

  /**
   * Abstract method to execute JavaScript asynchronously without returning any result to the
   * server.
   *
   * @param script The JavaScript code to execute asynchronously.
   * @return An integer identifier for the asynchronous execution.
   *
   * @throws BBjException if an error occurs during script execution.
   */
  protected abstract int doExecuteJsVoidAsync(String script) throws BBjException;
}
