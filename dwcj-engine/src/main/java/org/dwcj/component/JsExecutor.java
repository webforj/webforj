package org.dwcj.component;

import com.basis.startup.type.BBjException;
import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.dwcj.Page;
import org.dwcj.PendingResult;
import org.dwcj.concern.HasJsExecution;
import org.dwcj.exceptions.DwcjRuntimeException;

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
      scriptsStoreCopy.forEach(entry -> executeJsAsync(entry.getKey(), entry.getValue()));
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
        throw new DwcjRuntimeException(e);
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
    return executeJsAsync(script, null);
  }

  /**
   * Executes a provided JavaScript script asynchronously and returns a PendingResult.
   *
   * @param script The JavaScript code to execute asynchronously.
   * @param pending A {@link PendingResult} that will be completed when the script execution is
   *        finished.
   *
   * @return An {@link PendingResult} representing the pending result of the script execution.
   */
  private PendingResult<Object> executeJsAsync(String script, PendingResult<Object> pending) {
    PendingResult<Object> result = pending;
    if (result == null) {
      result = new PendingResult<>();
    }

    if (component.isAttached()) {
      try {
        if (!result.isCancelled()) {
          int id = doExecuteJsAsync(script);
          resultsStore.put(id, result);
        }
      } catch (BBjException e) {
        if (pending != null) {
          pending.completeExceptionally(e);
        }
      }
    } else {
      scriptsStore.add(new SimpleEntry<>(script, result));
    }

    return result;
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
}
