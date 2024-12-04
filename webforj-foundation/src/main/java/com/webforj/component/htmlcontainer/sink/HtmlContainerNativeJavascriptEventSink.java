package com.webforj.component.htmlcontainer.sink;

import com.basis.bbj.proxies.event.BBjNativeJavaScriptEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.htmlcontainer.HtmlContainer;
import com.webforj.component.htmlcontainer.event.HtmlContainerJavascriptEvent;

import com.basis.bbj.proxyif.SysGuiEventConstants;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class HtmlContainerNativeJavascriptEventSink {


  private final ArrayList<Consumer<HtmlContainerJavascriptEvent>> targets;
  private final HtmlContainer container;

  public HtmlContainerNativeJavascriptEventSink(HtmlContainer htmlv) {
    this.targets = new ArrayList<>();
    this.container = htmlv;

    BBjControl bbjctrl;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(htmlv);
      bbjctrl.setCallback(SysGuiEventConstants.ON_NATIVE_JAVASCRIPT,
          Environment.getCurrent().getBridge().getEventProxy(this, "onEvent"), "onEvent");
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public void onEvent(BBjNativeJavaScriptEvent ev) { // NOSONAR
    HtmlContainerJavascriptEvent dwcEv;
    try {
      dwcEv = new HtmlContainerJavascriptEvent(container, ev.getEventMap());
    } catch (BBjException e) {
      dwcEv = new HtmlContainerJavascriptEvent(container, null);
      App.consoleError("Error: could not determine JS event map!");
    }
    Iterator<Consumer<HtmlContainerJavascriptEvent>> it = targets.iterator();
    while (it.hasNext())
      it.next().accept(dwcEv);
  }

  public void addCallback(Consumer<HtmlContainerJavascriptEvent> callback) {
    targets.add(callback);
  }
}
