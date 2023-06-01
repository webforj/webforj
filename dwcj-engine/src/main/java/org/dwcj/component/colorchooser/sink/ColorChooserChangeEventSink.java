package org.dwcj.component.colorchooser.sink;

import com.basis.bbj.proxies.event.BBjColorChooserChangeEvent;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.basis.startup.type.BBjException;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.colorchooser.event.ColorChooserApproveEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.sink.AbstractSink;

import java.util.HashMap;

public class ColorChooserChangeEventSink extends AbstractSink {

  public ColorChooserChangeEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_COLORCHOOSER_CHANGE);
  }

  public void handleEvent(BBjEvent event) throws BBjException {
    BBjColorChooserChangeEvent ev = (BBjColorChooserChangeEvent) event;
    HashMap<String, Object> map = new HashMap<>();

    map.put("color", ev.getColor());

    ColorChooserApproveEvent dwcEv = new ColorChooserApproveEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
