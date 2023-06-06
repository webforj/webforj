package org.dwcj.component.colorchooser.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.colorchooser.event.ColorChooserCancelEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.sink.AbstractSink;

/**
 * Cancel EventSINK for the ColorChooser Component.
 */
public class ColorChooserCancelEventSink extends AbstractSink {

  public ColorChooserCancelEventSink(AbstractDwcComponent component, EventDispatcher dispatcher){
    super(component, dispatcher, SysGuiEventConstants.ON_COLORCHOOSER_CANCEL);
  }

  /**
  * taking care of Events on BBjSide and handing them on DWCj side.
  *
  * @param event using it to cast into BBjCancelEvent.
  * @throws BBjException to handle the event on BBj Side.
  */
  public void handleEvent(BBjEvent event) {
    HashMap<String, Object> map = new HashMap<>();
    ColorChooserCancelEvent dwcEv = new ColorChooserCancelEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
