package org.dwcj.component.maskedtextfield.sink;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.maskedtextfield.MaskedTextField;
import org.dwcj.component.maskedtextfield.event.MaskedTextFieldModifyEvent;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public class MaskedTextFieldModifyEventSink {

  private ArrayList<Consumer<MaskedTextFieldModifyEvent>> targets;
  private final MaskedTextField stringEditBox;


  @SuppressWarnings({"static-access"})
  public MaskedTextFieldModifyEventSink(MaskedTextField stringBox) {

    this.targets = new ArrayList<>();
    this.stringEditBox = stringBox;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(stringBox);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_EDIT_MODIFY,
          Environment.getCurrent().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
          "onEvent");

    } catch (Exception e) {
      Environment.logError(e);
    }
  }


  public void editModifyEvent(BBjEditModifyEvent ev) { // NOSONAR
    MaskedTextFieldModifyEvent dwcEv = new MaskedTextFieldModifyEvent(this.stringEditBox);
    Iterator<Consumer<MaskedTextFieldModifyEvent>> it = targets.iterator();
    while (it.hasNext())
      it.next().accept(dwcEv);
  }

  public void addCallback(Consumer<MaskedTextFieldModifyEvent> callback) {
    targets.add(callback);
  }



}
