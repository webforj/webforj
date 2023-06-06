package org.dwcj.component.colorchooser.sink;

import com.basis.bbj.proxies.event.BBjColorChooserChangeEvent;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.colorchooser.event.ColorChooserApproveEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.sink.AbstractSink;

/**
 * Approve EventSINK for the ColorChooser Component.
 */
public class ColorChooserChangeEventSink extends AbstractSink {

  public ColorChooserChangeEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_COLORCHOOSER_CHANGE);
  }

  /**
  * taking care of Events on BBjSide and handing them on DWCj side.
  *
  * @param event using it to cast into BBjChangeEvent.
  * @throws BBjException to handle the event on BBj Side.
  */  
  public void handleEvent(BBjEvent event) {
    BBjColorChooserChangeEvent ev = (BBjColorChooserChangeEvent) event;
    HashMap<String, Object> map = new HashMap<>();

    map.put("color", ev.getColor());

    ColorChooserApproveEvent dwcEv = new ColorChooserApproveEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}