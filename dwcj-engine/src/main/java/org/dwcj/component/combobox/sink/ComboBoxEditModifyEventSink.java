package org.dwcj.component.combobox.sink;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.combobox.ComboBox;
import org.dwcj.component.combobox.event.ComboBoxEditModifyEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public class ComboBoxEditModifyEventSink {

  private ArrayList<Consumer<ComboBoxEditModifyEvent>> targets = new ArrayList<>();
  private final ComboBox textComboBox;
  private BBjControl bbjctrl;


  @SuppressWarnings({"static-access"})
  public ComboBoxEditModifyEventSink(ComboBox cb) {
    this.textComboBox = cb;

    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(cb);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_EDIT_MODIFY,
          Environment.getCurrent().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
          "onEvent");
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  @SuppressWarnings({"static-access"})
  public ComboBoxEditModifyEventSink(ComboBox cb, Consumer<ComboBoxEditModifyEvent> callback) {
    this.targets.add(callback);
    this.textComboBox = cb;

    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(cb);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_EDIT_MODIFY,
          Environment.getCurrent().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
          "onEvent");
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  @SuppressWarnings("java.S1172")
  public void editModifyEvent(BBjEditModifyEvent ev) { // NOSONAR
    ComboBoxEditModifyEvent dwcEv = new ComboBoxEditModifyEvent(this.textComboBox);
    Iterator<Consumer<ComboBoxEditModifyEvent>> it = targets.iterator();
    while (it.hasNext())
      it.next().accept(dwcEv);
  }

  public void addCallback(Consumer<ComboBoxEditModifyEvent> callback) {
    targets.add(callback);
  }


}
