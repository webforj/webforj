package org.dwcj.component.button.sink;

import com.basis.bbj.proxies.event.BBjButtonPushEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.button.Button;
import org.dwcj.component.button.event.ButtonClickEvent;
import org.dwcj.component.events.EventDispatcher;
import java.util.HashMap;

public final class ButtonClickEventSink {

  private final Button button;
  private final EventDispatcher dispatcher;
  private boolean isActive = false;
  BBjControl bbjctrl = null;

  @SuppressWarnings({"static-access"})
  public ButtonClickEventSink(Button btn, EventDispatcher dispatcher) {
    this.dispatcher = dispatcher;
    this.button = btn;

    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(btn);
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public void addEvent() {
    if (!isActive) {
      isActive = true;
      try {
        bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_BUTTON_PUSH, // NOSONAR
            Environment.getInstance().getDwcjHelper().getEventProxy(this, "pushEvent"), "onEvent");
      } catch (Exception e) {
        Environment.logError(e);
      }
    }
  }

  public void removeEvent() {
    if (isActive && (dispatcher.getListenersCount(ButtonClickEvent.class) == 0)) {
      isActive = false;
      try {
        bbjctrl.clearCallback(Environment.getInstance().getBBjAPI().ON_BUTTON_PUSH);
      } catch (Exception e) {
        Environment.logError(e);
      }
    }
  }

  public void pushEvent(BBjButtonPushEvent ev) { // NOSONAR
    ButtonClickEvent dwcEv = new ButtonClickEvent(this.button, new HashMap<>());
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
