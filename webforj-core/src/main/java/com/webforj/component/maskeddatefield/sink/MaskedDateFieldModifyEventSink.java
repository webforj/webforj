package com.webforj.component.maskeddatefield.sink;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.webforj.Environment;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.maskeddatefield.MaskedDateField;
import com.webforj.component.maskeddatefield.event.MaskedDateFieldModifyEvent;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class MaskedDateFieldModifyEventSink {

  private ArrayList<Consumer<MaskedDateFieldModifyEvent>> targets;
  private final MaskedDateField dateEditBox;

  @SuppressWarnings({"static-access"})
  public MaskedDateFieldModifyEventSink(MaskedDateField dateBox) {

    this.targets = new ArrayList<>();
    this.dateEditBox = dateBox;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(dateBox);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_EDIT_MODIFY,
          Environment.getCurrent().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
          "onEvent");

    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public MaskedDateFieldModifyEventSink(MaskedDateField dateBox,
      Consumer<MaskedDateFieldModifyEvent> callback) {

    this.targets = new ArrayList<>();
    this.targets.add(callback);
    this.dateEditBox = dateBox;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(dateBox);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_EDIT_MODIFY,
          Environment.getCurrent().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
          "onEvent");

    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public void editModifyEvent(BBjEditModifyEvent ev) { // NOSONAR
    MaskedDateFieldModifyEvent dwcEv = new MaskedDateFieldModifyEvent(this.dateEditBox);
    Iterator<Consumer<MaskedDateFieldModifyEvent>> it = targets.iterator();
    while (it.hasNext())
      it.next().accept(dwcEv);
  }

  public void addCallback(Consumer<MaskedDateFieldModifyEvent> callback) {
    targets.add(callback);
  }
}
