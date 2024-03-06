package com.webforj.component.maskednumberfield.sink;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.webforj.Environment;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.maskednumberfield.MaskedNumberField;
import com.webforj.component.maskednumberfield.event.MaskedNumberFieldModifyEvent;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;


public class MaskedNumberFieldModifyEventSink {

  private ArrayList<Consumer<MaskedNumberFieldModifyEvent>> targets;
  private final MaskedNumberField numericBox;

  @SuppressWarnings({"static-access"})
  public MaskedNumberFieldModifyEventSink(MaskedNumberField numBox) {

    this.targets = new ArrayList<>();
    this.numericBox = numBox;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(numBox);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_EDIT_MODIFY,
          Environment.getCurrent().getWeforjHelper().getEventProxy(this, "editModifyEvent"),
          "onEvent");

    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public MaskedNumberFieldModifyEventSink(MaskedNumberField numBox,
      Consumer<MaskedNumberFieldModifyEvent> callback) {

    this.targets = new ArrayList<>();
    this.targets.add(callback);
    this.numericBox = numBox;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(numBox);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_EDIT_MODIFY,
          Environment.getCurrent().getWeforjHelper().getEventProxy(this, "editModifyEvent"),
          "onEvent");

    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public void editModifyEvent(BBjEditModifyEvent ev) { // NOSONAR
    MaskedNumberFieldModifyEvent dwcEv = new MaskedNumberFieldModifyEvent(this.numericBox);
    Iterator<Consumer<MaskedNumberFieldModifyEvent>> it = targets.iterator();
    while (it.hasNext())
      it.next().accept(dwcEv);
  }

  public void addCallback(Consumer<MaskedNumberFieldModifyEvent> callback) {
    targets.add(callback);
  }
}
