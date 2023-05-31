package org.dwcj.component.htmledit.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjPageLoadedEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.basis.startup.type.BBjException;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.sink.AbstractSink;
import org.dwcj.component.htmledit.event.PageLoadedEvent;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * This class will map the BBjPageLoaded event to a Java {@link PageLoadedEvent}.
 */
public class PageLoadedEventSink extends AbstractSink {

  public PageLoadedEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_PAGE_LOADED);
  }

  /**
   * Handles the BBj event and dispatches a new {@link PageLoadedEvent}.
   *
   * @param ev A BBj page loaded event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjPageLoadedEvent event = (BBjPageLoadedEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    try {
      map.put("text", event.getText());
      map.put("url", event.getUrl());
    } catch (BBjException e) {
      throw new DwcjRuntimeException(e);
    }
    PageLoadedEvent dwcEv = new PageLoadedEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
