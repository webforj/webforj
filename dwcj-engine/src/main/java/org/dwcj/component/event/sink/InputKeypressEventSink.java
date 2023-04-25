package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.InputKeypressEvent;

/**
 * This class will map the BBjInputKeypressEvent event to a Java {@link InputKeypressEvent}.
 */
public class InputKeypressEventSink extends AbstractKeypressEventSink {

  public InputKeypressEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_INPUT_KEYPRESS);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    HashMap<String, Object> map = super.buildPayload(ev);

    InputKeypressEvent dwcEv = new InputKeypressEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
