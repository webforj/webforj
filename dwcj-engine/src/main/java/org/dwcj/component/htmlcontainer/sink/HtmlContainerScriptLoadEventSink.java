package org.dwcj.component.htmlcontainer.sink;

import com.basis.bbj.proxies.event.BBjScriptLoadedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxyif.SysGuiEventConstants;


import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.htmlcontainer.HtmlContainer;
import org.dwcj.component.htmlcontainer.event.HtmlContainerScriptLoadEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public class HtmlContainerScriptLoadEventSink {

  private ArrayList<Consumer<HtmlContainerScriptLoadEvent>> targets;
  private final HtmlContainer htmlContainer;

  public HtmlContainerScriptLoadEventSink(HtmlContainer container) {

    this.targets = new ArrayList<>();
    this.htmlContainer = container;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(container);
      bbjctrl.setCallback(SysGuiEventConstants.ON_SCRIPT_LOADED,
          Environment.getCurrent().getDwcjHelper().getEventProxy(this, "scriptLoadedEvent"),
          "onEvent");

    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public void scriptLoadedEvent(BBjScriptLoadedEvent ev) { // NOSONAR
    HtmlContainerScriptLoadEvent dwcEv = new HtmlContainerScriptLoadEvent(this.htmlContainer);
    Iterator<Consumer<HtmlContainerScriptLoadEvent>> it = targets.iterator();
    while (it.hasNext())
      it.next().accept(dwcEv);
  }

  public void addCallback(Consumer<HtmlContainerScriptLoadEvent> callback) {
    targets.add(callback);
  }
}
