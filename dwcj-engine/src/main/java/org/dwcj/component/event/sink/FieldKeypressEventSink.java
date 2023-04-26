package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.FieldKeypressEvent;
import org.dwcj.component.field.Field;

/**
 * This class will map the BBjInputKeypressEvent and BBjEditKeyPressEvent event to a Java
 * {@link FieldKeypressEvent}.
 */
public class FieldKeypressEventSink extends AbstractKeypressEventSink {

  public FieldKeypressEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, component instanceof Field ? SysGuiEventConstants.ON_EDIT_KEYPRESS
        : SysGuiEventConstants.ON_INPUT_KEYPRESS);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    HashMap<String, Object> map = super.buildPayload(ev);

    FieldKeypressEvent dwcEv = new FieldKeypressEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
