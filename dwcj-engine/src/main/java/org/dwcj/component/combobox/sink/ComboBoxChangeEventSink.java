package org.dwcj.component.combobox.sink;

import com.basis.bbj.proxies.event.BBjListChangeEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.combobox.ComboBox;
import org.dwcj.component.combobox.event.ComboBoxChangeEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;



public class ComboBoxChangeEventSink {

  private ArrayList<Consumer<ComboBoxChangeEvent>> targets = new ArrayList<>();

  private final ComboBox textComboBox;

  private BBjControl bbjctrl;

  @SuppressWarnings({"static-access"})
  public ComboBoxChangeEventSink(ComboBox cb) {
    this.textComboBox = cb;

    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(cb);
      bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_CHANGE,
          Environment.getInstance().getDwcjHelper().getEventProxy(this, "changeEvent"), "onEvent");
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  @SuppressWarnings({"static-access"})
  public ComboBoxChangeEventSink(ComboBox cb, Consumer<ComboBoxChangeEvent> callback) {
    this.targets.add(callback);
    this.textComboBox = cb;

    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(cb);
      bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_CHANGE,
          Environment.getInstance().getDwcjHelper().getEventProxy(this, "changeEvent"), "onEvent");
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  @SuppressWarnings("java.S1172")
  public void changeEvent(BBjListChangeEvent ev) { // NOSONAR
    ComboBoxChangeEvent dwcEv = new ComboBoxChangeEvent(this.textComboBox);
    Iterator<Consumer<ComboBoxChangeEvent>> it = targets.iterator();
    while (it.hasNext())
      it.next().accept(dwcEv);
  }

  public void addCallback(Consumer<ComboBoxChangeEvent> callback) {
    targets.add(callback);
  }

}
