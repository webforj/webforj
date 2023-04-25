package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EditModifyEvent;
import org.dwcj.component.event.EventDispatcher;

/**
 * This class will map the BBjEditModifyEvent event to a Java {@link EditModifyEvent}.
 */
public class EditModifyEventSink extends AbstractSink {

  protected EditModifyEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_EDIT_MODIFY);
  }
  
  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjEditModifyEvent event = (BBjEditModifyEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("text", event.getText());

    EditModifyEvent dwcEv = new EditModifyEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }

}
