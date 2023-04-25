package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EditKeypressEvent;
import org.dwcj.component.event.EventDispatcher;

/**
 * This class will map the BBjEditKeypressEvent event to a Java {@link EditKeypressEvent}.
 */
public class EditKeypressEventSink extends AbstractKeypressEventSink {

  public EditKeypressEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_EDIT_KEYPRESS);
  }
  
  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    HashMap<String, Object> map = super.buildPayload(ev);

    EditKeypressEvent dwcEv = new EditKeypressEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
