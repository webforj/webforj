package org.dwcj.component.htmlcontainer.sink;

import com.basis.bbj.proxies.event.BBjPageLoadedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.htmlcontainer.HtmlContainer;
import org.dwcj.component.htmlcontainer.event.HtmlContainerPageLoadEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class HtmlContainerPageLoadEventSink {

  private final ArrayList<Consumer<HtmlContainerPageLoadEvent>> targets;
  private final HtmlContainer container;

  public HtmlContainerPageLoadEventSink(HtmlContainer htmlv) {
    this.targets = new ArrayList<>();
    this.container = htmlv;

    BBjControl bbjctrl = null;

    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(htmlv);
      bbjctrl.setCallback(SysGuiEventConstants.ON_PAGE_LOADED,
          Environment.getInstance().getDwcjHelper().getEventProxy(this, "onEvent"), "onEvent");
    } catch (Exception e) {
      Environment.logError(e);
    }

  }

  public void onEvent(BBjPageLoadedEvent ev) { // NOSONAR
    HtmlContainerPageLoadEvent dwcEv = new HtmlContainerPageLoadEvent(container);
    Iterator<Consumer<HtmlContainerPageLoadEvent>> it = targets.iterator();
    while (it.hasNext())
      it.next().accept(dwcEv);
  }

  public void addCallback(Consumer<HtmlContainerPageLoadEvent> callback) {
    targets.add(callback);
  }


}
