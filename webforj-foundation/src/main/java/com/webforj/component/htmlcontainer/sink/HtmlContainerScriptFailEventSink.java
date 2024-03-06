package com.webforj.component.htmlcontainer.sink;

import com.basis.bbj.proxies.event.BBjScriptFailedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.webforj.Environment;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.htmlcontainer.HtmlContainer;
import com.webforj.component.htmlcontainer.event.HtmlContainerScriptFailEvent;

import com.basis.bbj.proxyif.SysGuiEventConstants;


import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;



public class HtmlContainerScriptFailEventSink {

  private ArrayList<Consumer<HtmlContainerScriptFailEvent>> targets;
  private final HtmlContainer htmlContainer;

  public HtmlContainerScriptFailEventSink(HtmlContainer container) {

    this.targets = new ArrayList<>();
    this.htmlContainer = container;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(container);
      bbjctrl.setCallback(SysGuiEventConstants.ON_SCRIPT_FAILED,
          Environment.getCurrent().getDwcjHelper().getEventProxy(this, "scriptFailedEvent"),
          "onEvent");

    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public void scriptFailedEvent(BBjScriptFailedEvent ev) { // NOSONAR
    HtmlContainerScriptFailEvent dwcEv = new HtmlContainerScriptFailEvent(this.htmlContainer);
    Iterator<Consumer<HtmlContainerScriptFailEvent>> it = targets.iterator();
    while (it.hasNext())
      it.next().accept(dwcEv);
  }

  public void addCallback(Consumer<HtmlContainerScriptFailEvent> callback) {
    targets.add(callback);
  }

}
