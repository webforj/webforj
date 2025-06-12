package com.webforj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjExecuteScriptEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.DwcComponent;
import com.webforj.component.event.ExecuteAsyncScriptEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;

/**
 * This class will map the BBjExecuteScriptEvent event to a {@link ExecuteAsyncScriptEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public final class ExecuteAsyncScriptEventSink extends AbstractDwcEventSink {

  public ExecuteAsyncScriptEventSink(DwcComponent<?> component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_EXECUTE_SCRIPT);
  }

  /**
   * Handles the BBj event and dispatches a new {@link ExecuteAsyncScriptEvent}.
   *
   * @param ev the BBj event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjExecuteScriptEvent event = (BBjExecuteScriptEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("index", event.getIndex());
    map.put("result", event.getResult());

    ExecuteAsyncScriptEvent dwcEv = new ExecuteAsyncScriptEvent(getComponent(), map);
    getEventDispatcher().dispatchEvent(dwcEv);
  }
}
