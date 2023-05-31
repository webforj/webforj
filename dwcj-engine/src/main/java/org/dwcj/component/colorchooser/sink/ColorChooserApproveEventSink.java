package org.dwcj.component.colorchooser.sink;

import com.basis.bbj.proxies.event.BBjColorChooserApproveEvent;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.sysgui.BBjColorChooser;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.basis.startup.type.BBjException;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.colorchooser.event.ColorChooserApproveEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.sink.AbstractSink;

import java.awt.*;
import java.util.HashMap;

public class ColorChooserApproveEventSink extends AbstractSink {

  public ColorChooserApproveEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_COLORCHOOSER_APPROVE);
  }

  public void handleEvent(BBjEvent event) throws BBjException {
    BBjColorChooserApproveEvent ev = (BBjColorChooserApproveEvent) event;
    HashMap<String, Object> map = new HashMap<>();

    map.put("color", ev.getColor());

    ColorChooserApproveEvent dwcEv = new ColorChooserApproveEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }

}
