package org.dwcj.component.colorchooser.sink;

import com.basis.bbj.proxies.event.BBjColorChooserCancelEvent;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.colorchooser.event.ColorChooserCancelEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.sink.AbstractSink;

import java.util.HashMap;

public class ColorChooserCancelEventSink extends AbstractSink {

  public ColorChooserCancelEventSink(AbstractDwcComponent component, EventDispatcher dispatcher){
    super(component, dispatcher, SysGuiEventConstants.ON_COLORCHOOSER_CANCEL);
  }

  public void handleEvent(BBjEvent event) {
    HashMap<String, Object> map = new HashMap<>();
    ColorChooserCancelEvent dwcEv = new ColorChooserCancelEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }

}
